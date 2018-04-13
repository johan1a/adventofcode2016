(ns day24.core-test
  (:require [clojure.test :refer :all]
            [day24.core :refer :all]
            [day24.path-finding :refer :all]))

(deftest easy input
  (is (= 8 (distance-between tinput [1 1] [1 9] )))
  (is (= 10 (distance-between tinput [1 1] [3 9] )))
  (is (= 1 (distance-between tinput [1 1] [1 2] ))))
