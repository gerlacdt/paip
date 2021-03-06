(ns paip.search)

(defn tree-search
  "General tree-search function"
  [states goal?-fn successors combiner]
  (cond (empty? states) nil
        (goal?-fn (first states)) (first states)
        :else (recur (combiner (successors (first states))
                                     (rest states))
                           goal?-fn
                           successors
                           combiner)))

(defn prepend [lst-1 lst-2]
  (concat lst-2 lst-1))

(defn breadth-first-search
  [start goal?-fn successors]
  (tree-search (list start) goal?-fn successors prepend))

(defn depth-first-search
  [start goal?-fn successors]
  (tree-search (list start) goal?-fn successors concat))


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
        :else (recur (combiner
                             (new-states states successors old-states)
                             (rest states))
                           goal?-fn
                           successors
                           combiner
                           (conj (set old-states) (first states)))))


;; best-first-search
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
