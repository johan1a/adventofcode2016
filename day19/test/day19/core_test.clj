(ns day19.core-test
  (:require [clojure.test :refer :all]
            [day19.core :refer :all]))

(deftest a-test
  (testing "Test example game"
    (is (= 3 (:number (play-game (make-gnomes 5)))))))
