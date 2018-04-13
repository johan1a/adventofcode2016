(ns day24.core-test
  (:require [clojure.test :refer :all]
            [day24.core :refer :all]
            [day24.path-finding :refer :all]))

(deftest easy
  (is (= 8 (distance-between tinput [1 1] [1 9] )))
  (is (= 10 (distance-between tinput [1 1] [3 9] )))
  (is (= 1 (distance-between tinput [1 1] [1 2] ))))

(deftest update1
  (is (= {[0 0] {[1 0] 1, [5 8] 1230}}
         (update-dists-map {} [0 0] [[1 0], [5 8]] [1 1230]))))

(deftest get-distmap
  (is (= {[1 1] {[1 3] 2, [1 9] 8, [3 9] 10, [3 1] 2}, [1 3] {[1 9] 6, [3 9] 8, [3 1] 4}, [1 9] {[3 9] 2, [3 1] 10}, [3 9] {[3 1] 8}}
         (get-all-dists tinput [1 1] (get-targets tinput 5)))))

