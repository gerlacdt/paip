(ns paip.gps-enhanced
  (:require [clojure.set :as set]))

(defrecord Operation [action preconds add-list del-list])

(def ops (list (Operation. 'drive-son-to-school '(son-at-home car-works) '(son-at-school) '(son-at-home))
               (Operation. 'shop-installs-battery '(car-needs-battery shop-knows-problem shop-has-money) '(car-works) nil)
               (Operation. 'tell-shop-problem '(in-communication-with-shop) '(shop-knows-problem) nil)
               (Operation. 'telephone-shop '(know-phone-number) '(in-communication-with-shop) nil)
               (Operation. 'look-up-number '(have-phone-book) '(know-phone-number) nil)
               (Operation. 'give-shop-money '(have-money) '(shop-has-money) '(have-money))))


(defn starts-with
  "Is this a list whose first element is x?"
  [list x]
  (and (sequential? list) (= x (first list))))

(defn executing-p
  "Is x of form (executing ...)?"
  [x]
  (starts-with x 'executing))

(defn convert-op
  "Make op conform to the (EXECUTING op) convention."
  [op]
  (if (some executing-p (:add-list op))
    op
    (do
      (Operation. (:action op) (:preconds op)
                  (cons (list 'executing (:action op)) (:add-list op))
                  (:del-list op)))))

(def eops (map convert-op ops))


(declare achieve)
(declare achieve-all)

(defn member-equal [item list]
  (some #(= item %) list))


(defn appropriate-p
  "An op is appropriate to a goal if it is i its add list"
  [goal op]
  (member-equal goal (:add-list op)))

(defn apply-op
  "Return a new, transformed state if op is applicable"
  [state goal op goal-stack operations]
  (let [state2 (achieve-all state (:preconds op) (cons goal goal-stack) operations)]
    (when state2
      (concat (remove (fn [x] (member-equal x (:del-list op))) state2)
              (:add-list op)))))

(defn achieve
  "A goal is achieved if it already holds, or if there is an appropriate op for it
  that is applicable."
  [state goal goal-stack operations]
  (cond (member-equal goal state) state
        (member-equal goal goal-stack) nil
        :else (some (fn [op] (apply-op state goal op goal-stack operations))
                    (filter (partial appropriate-p goal) operations))))

(defn achieve-all
  "achieve each goal, and make sure they still hold at the end"
  [state goals goal-stack operations]
  (let [current-state (atom state)]
    (if (and (every?
              (fn [g] (reset! current-state (achieve @current-state g goal-stack operations))) goals)
             (set/subset? (set goals) (set @current-state)))
      @current-state)))

(defn gps
  "general problem solver algorithm"
  [state goals operations]
  (let [result (achieve-all (cons '(start) state) goals nil operations)]
    (filter #(or (= '(start) %) (and (sequential? %) (= 'executing (first %)))) result)))

(defn solve-easy []
  (let [state '(son-at-home car-works)]
    (gps state '(son-at-school) eops)))

(defn solve-hard []
  (let [state '(son-at-home car-needs-battery have-money have-phone-book)]
    (gps state '(son-at-school) eops)))
