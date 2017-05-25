(ns day19.core-test
  (:require [clojure.test :refer :all]
            [day19.core :refer :all]))

(deftest a-test
  (testing "Test example game"
    (is (= 3 (play-game (make-gnomes 5))))))

(deftest test-part-one
  (is (= 1808357 (part-one))))

(deftest test-part-two-example
  (is (= 2 (play-game2 (make-gnomes-m 5)))))


