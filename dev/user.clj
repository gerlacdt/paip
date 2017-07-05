(ns user
  (:require [clojure.repl :refer :all]
            [clojure.tools.trace :as trace]))

(defn clear-test-ns
  "Clears the given namespace"
  [ns]
  (doseq [func (keys (ns-publics ns))]
    (ns-unmap ns func)))
