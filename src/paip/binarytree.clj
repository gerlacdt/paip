(ns paip.binarytree
  (:require [paip.search :as search])
  (:import [paip.search Path]))


(defn diff
  [num]
  (fn [x]
    (Math/abs (- x num))))

(defn price-is-right
  "Return a function that measures the difference from price"
  [price]
  (fn [x]
    (if (> x price)
      (Integer/MAX_VALUE)
      (- price x))))


(defn binary-tree
  "A successor function"
  [x]
  {:pre [(pos? x)]
   :post [(= (count %) 2)]}
  (list (* 2 x) (+ 1 (* 2 x))))

(defn finite-binary-tree
  "Returns a successor function that generates a tree with n nodes"
  [n]
  (fn [x]
    (filter #(<= % n) (binary-tree x))))

(defn abs
  [x]
  (Math/abs x))

(defn breadth-search-binary-tree-path
  [n]
  (search/breadth-first-search (Path. 1 nil 0 n)
                        #(= (:state %1) n)
                        (search/path-saver binary-tree (fn [_ _] 1) (fn [state] (abs (- state n))))))

(defn depth-search-binary-tree-path
  [n]
  (search/depth-first-search (Path. 1 nil 0 n)
                      #(= (:state %1) n)
                      (search/path-saver (finite-binary-tree n) (fn [_ _] 1) (fn [state] (abs (- state n))))))

(defn is
  [n]
  (fn [x] (= x n)))
