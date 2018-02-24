(ns paip.eliza
  (:require [clojure.string :as str])
  (:use [spyscope.core]
        [clojure.test]))


(defn lookup
  "Find a binding with the given var in the given binding list."
  [key bindings]
  (key bindings))

(defn extend-binding
  "Add a binding with the given key to bindings"
  [key val bindings]
  (merge bindings {key val}))


(defn variable-p
  "Is x a variable (a symbol beginning with ?)?"
  [x]
  (and (symbol? x) (str/starts-with? (name x) "?")))

(defn match-variable
  "Does var match input? Uses or updates and returns bindings."
  [var input bindings]
  (let [value (lookup var bindings)]
    (cond (nil? value) (extend-binding var input bindings)
          (= input value) bindings
          :else nil)))


(defn position
  "Returns the index of the first item in the given list. Start searching from
  given start index."
  [item lst start]
  (+ start (.indexOf (drop start lst) item)))


(declare segment-pattern-p)
(declare segment-match)

(defn pat-match
  "Match pattern agains input in the context of the bindings."
  ([pattern input] (pat-match pattern input {}))
  ([pattern input bindings]
   (cond (nil? bindings) nil
         (variable-p pattern) (match-variable pattern input bindings)
         (= pattern input) bindings
         (segment-pattern-p pattern) (segment-match pattern input bindings)
         (and (sequential? pattern) (sequential? input)) (pat-match (rest pattern) (rest input)
                                                                    (pat-match (first pattern) (first input) bindings))
         :else nil)))

(defn segment-pattern-p
  "Is this a segment matching pattern: ((?* var) pat)"
  [pattern]
  (and (sequential? pattern) (sequential? (first pattern)) (= '?* (first (first pattern)))))

(defn segment-match
  "Match the segment pattern ((?* var) pattern) against input"
  ([pattern input bindings] (segment-match pattern input bindings 0))
  ([pattern input bindings start]
   (let [var (second (first pattern))
         pat (rest pattern)]
     (if (or (nil? pat) (= pat '()))
       (match-variable var input bindings)
       ;; we assume that pat starts with a constant
       ;; In other words, a pattern can't have 2 consecutive vars
       (let [pos (position (first pat) input start)]
         (if (= -1 pos)
           nil
           (let [b2 (pat-match pat (drop pos input)
                               (match-variable var (take pos input) bindings))]
             ;; If this match failed, try another longer one
             ;; If it worked, check that the variables match
             (if (nil? b2)
               (segment-match pattern input bindings (+ pos 1))
               b2))))))))

;; call with (run-tests 'paip.eliza) in namespace
(deftest test-segment-match
  (is (= {'?X 'vacation} (pat-match '(i need a ?X) '(i need a vacation))))
  (is (= {} (pat-match '(this is easy) '(this is easy))))
  (is (= {'?X '(2 + 2)} (pat-match '(?X is ?X) '((2 + 2) is (2 + 2)))))
  (is (= {'?p '(Mr Hulot and I), '?x '(a vacation)} (pat-match '((?* ?p) need (?* ?x))
                                                               '(Mr Hulot and I need a vacation))))
  (is (= {'?x '(), '?y '(there)} (pat-match '((?* ?x) hello (?* ?y))
                                                               '(hello there))))
  (is (= {'?x '(what he is), '?y '(fool)} (pat-match '((?* ?x) is a (?* ?y)) '(what he is is a fool)) ))
  (is (= {'?x '(1 2 a b)} (pat-match '((?* ?x) a b (?* ?x))
                                     '(1 2 a b a b 1 2 a b)))))

(def ^{:private true}
  *eliza-rules*
  '((((?* ?x) hello (?* ?y))
     (How do you do. Please state your problem.))
    (((?* ?x) I want (?* ?y))
     (What would it mean if you got ?y ?)
     (Why do you want ?y ?)
     (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y))
     (Do you really think it is likely that ?y ?)
     (Do you wish that ?y ?)
     (What do you think about ?y ?)
     (Really? If ?y ?))
    (((?* ?x) no (?* ?y))
     (Why not?) (You are being rather negative.)
     (Are you saying no just to be negative?))
    (((?* ?x) I was (?* ?y))
     (Were you really?) (Perhaps I already knew you were $y .)
     (Why do you tell me you were ?y now?))
    (((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?y))
     (What other feelings do you have?))
    (((?* ?x))
     (I am not sure I understand.)
     (Could you put that another way?)
     (Go on.))))

(defn rule-pattern [rule]
  (first rule))

(defn rule-responses [rule]
  (rest rule))


(def ^{:private true}
  *viewpoint-map* {'I 'you 'you 'I 'me 'you 'am 'are})

(defn- switch-viewpoint
  "Change I to you and vice versa, and so on."
  [bindings]
  (reduce (fn [m pair]
            (assoc m (first pair)
                   (map #(*viewpoint-map* % %) (second pair))))
          {} bindings))

(defn use-eliza-rules
  "Find some rule with which to transform the input."
  [input]
  (some #(let [result (pat-match (rule-pattern %) input)]
           (when result
             (replace (switch-viewpoint result)
                      (rand-nth (rule-responses %)))))
        *eliza-rules*))

(defn eliza
  "Respond to user input using pattern matching rules."
  []
  (print "eliza> ")
  (flush)
  (let [input (read)]
    (if (= input '(exit))
      'DONE
      (do (println (flatten (use-eliza-rules (read))))
          (recur)))))
