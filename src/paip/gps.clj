(ns paip.gps
  (:require [clojure.set :as set]))

(defrecord Operation [action preconds add-list del-list])

(def state (atom #{'son-at-home 'car-needs-battery
                   'have-money 'have-phone-book}))


(def ops (atom [(Operation. 'drive-son-to-school #{'son-at-home 'car-works} #{'son-at-school} #{'son-at-home})
                (Operation. 'shop-installs-battery #{'car-needs-battery 'shop-knows-problem 'shop-has-money} #{'car-works} nil)
                (Operation. 'tell-shop-problem #{'in-communication-with-shop} #{'shop-knows-problem} nil)
                (Operation. 'telephone-shop #{'know-phone-number} #{'in-communication-with-shop} nil)
                (Operation. 'look-up-number #{'have-phone-book} #{'know-phone-number} nil)
                (Operation. 'give-ship-money #{'have-money} #{'shop-has-money} #{'have-money})]))

(defn gps
  "general problem solver algorithm"
  [goals]
  (if (every? achieve goals)
    'solved
    nil))

(defn achieve
  "A goal is achieved if it already holds, or if there is an appropriate op for it
  that is applicable."
  [goal]
  (or (some #(= goal %) @state)
      (some apply-op (filter (partial appropriate-p goal) @ops))))


(defn appropriate-p
  "An op is appropriate to a goal if it is i its add list"
  [goal op]
  (some #(= goal %) (:add-list op)))

(defn apply-op
  "Print message and update state if op is applicable"
  [op]
  (when (every? achieve (:preconds op))
    (println (list 'executing (:action op)))
    (reset! state (set/difference @state (:del-list op)))
    (reset! state (set/union @state (:add-list op)))
    true))


(defn solve []
  (gps #{'son-at-school}))
