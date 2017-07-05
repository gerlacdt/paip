(ns paip.search)

(defn fibonacci
  "fibonacci function"
  [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fibonacci (- n 2)) (fibonacci (- n 1)))))


(defn tree-search
  "General tree-search function"
  [states goal?-fn successors combiner]
  (cond (empty? states) nil
        (goal?-fn (first states)) (first states)
        :else (tree-search (combiner (successors (first states))
                                     (rest states))
                           goal?-fn
                           successors
                           combiner)))


;; graph-search

(defn new-states
  "Generate successor states that have not have seen before"
  [states successors old-states]
  (remove (fn [state]
            (or (contains? (set states) state)
                (contains? (set old-states) state)))
          (successors (first states))))

(defn graph-search
  "General graph-search, similar to tree-search but holds all explored states.
  Don't try the same state twice."
  [states goal?-fn successors combiner old-states]
  (cond (empty? states) nil
        (goal?-fn (first states)) (first states)
        :else (graph-search (combiner
                             (new-states states successors old-states)
                             (rest states))
                           goal?-fn
                           successors
                           combiner
                           (conj (set old-states) (first states)))))


(defn prepend [lst-1 lst-2]
  (concat lst-2 lst-1))

(defn breadth-first-search
  [start goal?-fn successors]
  (tree-search (list start) goal?-fn successors prepend))

(defn depth-first-search
  [start goal?-fn successors]
  (tree-search (list start) goal?-fn successors concat))

;; best-first-search

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

(defn sorter
  [cost-fn]
  (fn [new old]
    (sort-by cost-fn (concat new old))))

(defn best-fist-search
  [start goal?-fn successors cost-fn]
  (tree-search (list start) goal?-fn successors (sorter cost-fn)))

;; beam-search
(defn beam-search
  [start goal?-fn successors cost-fn beam-width]
  (tree-search (list start) goal?-fn successors
               (fn [old new]
                 (let [sorted ((sorter cost-fn) old new)]
                   (if (> beam-width (count sorted))
                     sorted
                     (take beam-width sorted))))))


;; return all solutions
(defn search-all
  "Find all solutions to a search problem, using beam-search.
  Be careful this can lead to an infinite loop"
  [start goal?-fn successors]
  (with-local-vars [solutions '()]
    (depth-first-search start
                 (fn [x]
                   (when (goal?-fn x)
                     (var-set solutions (conj @solutions x)))
                   nil)
                 successors)
    @solutions))

;; save search path

(defrecord Path
    [state previous cost-so-far total-cost])

(defn path-saver
  "Returns an enriched successor function which save the path"
  [successors cost-fn cost-left-fn]
  (fn [old-path]
    (let [old-state (:state old-path)
          f (fn [new-state]
              (let [old-cost (+ (:cost-so-far old-path)
                                (cost-fn old-state new-state))
                    total-cost (+ old-cost (cost-left-fn new-state))]
                (Path. new-state old-path old-cost total-cost)))]
      (map f (successors old-state)))))

;; binary-tree example
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
  (breadth-first-search (Path. 1 nil 0 n)
                        #(= (:state %1) n)
                        (path-saver binary-tree (fn [_ _] 1) (fn [state] (abs (- state n))))))

(defn depth-search-binary-tree-path
  [n]
  (depth-first-search (Path. 1 nil 0 n)
                        #(= (:state %1) n)
                        (path-saver (finite-binary-tree n) (fn [_ _] 1) (fn [state] (abs (- state n))))))

(defn is
  [n]
  (fn [x] (= x n)))

(defn next2
  [x]
  (list (+ x 1) (+ x 2)))
