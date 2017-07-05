(ns paip.search-test
  (:require [paip.search :as sut]
            [clojure.test :refer :all]))


(deftest depth-first-search
  (testing "depth-first-search"
    (is (= 12 (sut/depth-first-search 1 (sut/is 12) (sut/finite-binary-tree 20))))))

(deftest breath-first-search
  (testing "breath-first-search"
    (is (= 12 (sut/breadth-first-search 1 (sut/is 12) sut/binary-tree)))))

(deftest best-first-search
  (testing "best-first-search"
    (is (= 12 (sut/best-fist-search 1 (sut/is 12) sut/binary-tree (sut/diff 12))))
    (is (= 12 (sut/best-fist-search 1 (sut/is 12) sut/binary-tree (sut/price-is-right 12))))))

(deftest beam-search
  (testing "beam-search"
    (is (= 12 (sut/beam-search 1 (sut/is 12) sut/binary-tree (sut/price-is-right 12) 2)))))

(deftest graph-search
  (testing "graph-search"
    (is (= 12 (sut/graph-search '(1) (sut/is 12) (sut/finite-binary-tree 20) concat #{})))))

(deftest search-all
  (testing "search-all"
    (is (= '(12) (sut/search-all 1 (sut/is 12) (sut/finite-binary-tree 20))))))

(deftest breadth-search-path
  (testing "with path"
    (let [result (sut/breadth-search-binary-tree-path 12)]
      (is (= 12 (:state result)))
      (is (= 3 (:cost-so-far result))))))

(deftest depth-search-path
  (testing "with path"
    (let [result (sut/depth-search-binary-tree-path 12)]
      (is (= 12 (:state result)))
      (is (= 3 (:cost-so-far result))))))
