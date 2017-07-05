(ns paip.nqueens
  (:require [paip.search :refer :all]))

;; (breadth-first-search 1 (is 12) binary-tree)

(defn abs
  [x]
  (Math/abs x))

(defn conflict?
  "Returns true if queen can be placed without any conflict with other queen in
  the board."
  [queen-1 queen-2]
  (or
   (= (:x queen-1) (:x queen-2))
   (= (:y queen-1) (:y queen-2))
   (= (abs (- (:x queen-1) (:x queen-2)))
      (abs (- (:y queen-1) (:y queen-2))))))


(defn conflicts?
  "Return true if there is a conflict within states."
  [states]
  (let [new-state (last states)
        old-states (butlast states)]
    (some (fn [old-state]
              (conflict? old-state new-state))
            old-states)))

(defn successors
  "Returns all next states which place a non-conflicting queen at the board"
  [n]
  (fn [state]
    (cond
      (empty? state) (map (fn [y] (list {:x 0 :y y})) (range n))
      (= n (count state)) nil
      :else
      (let [x (+ 1 (:x (apply max-key :x state)))]
        (filter (complement conflicts?)
                (map (fn [y] (concat state (list {:x x :y y}))) (range n)))))))


(defn nqueens-breadth-search
  [n]
  (breadth-first-search [] (fn [states] (= n (count states))) (successors n)))

(defn nqueens-depth-search
  [n]
  (depth-first-search [] (fn [states] (= n (count states))) (successors n)))

(defn nqueens-all-solutions
  [n]
  (search-all [] (fn [states] (= n (count states))) (successors n)))


;; (count (nqueens-all-solutions 4))
;; (count (nqueens-all-solutions 8))
