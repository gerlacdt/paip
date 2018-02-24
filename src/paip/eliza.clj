(ns paip.eliza
  (:require [clojure.string :as str])
  (:use [spyscope.core]))


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
       (let [pos (.indexOf (drop start input) (first pat))]
         (if (= -1 pos)
           nil
           (let [b2 (pat-match pat (drop pos input) bindings)]
             ;; If this match failed, try another longer one
             ;; If it worked, check that the variables match
             (if (nil? b2)
               (segment-match pattern input bindings (+ pos 1))
               (match-variable var (take pos input) b2)))))))))

(defn solve-1 []
  (pat-match '(i need a ?X) '(i need a vacation)))

(defn solve-2 []
  (pat-match '(this is easy) '(this is easy)))

(defn solve-3 []
  (pat-match '(?X is ?X) '((2 + 2) is (2 + 2))))

(defn solve-4 []
  (pat-match '((?* ?p) need (?* ?x))
             '(Mr Hulot and I need a vacation)))

;;  infinite recursion (somehow the recursion stop condition is wrong)
(defn solve-5 []
  (pat-match '((?* ?x) is a (?* ?y)) '(what he is is a fool)))
