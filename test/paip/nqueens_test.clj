(ns paip.nqueens-test
  (:require [paip.nqueens :refer :all]
            [clojure.test :refer :all]))

(deftest nqueens
  (testing "find all solutions."
    (is (= 2 (count (nqueens-all-solutions 4))))
    (is (= 92 (count (nqueens-all-solutions 8))))))
