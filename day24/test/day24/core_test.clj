(ns day24.core-test
  (:require [clojure.test :refer :all]
            [day24.core :refer :all]
            [day24.path-finding :refer :all]))

(deftest a-test
  (is (= 8 (distance-between tinput [1 1] [1 9] ))))
