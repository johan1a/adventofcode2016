(ns day21.core-test
  (:require [clojure.test :refer :all]
            [day21.core :refer :all]))

(deftest test-part-one
  []
  (is "decab" (exec-file test-initial "test-input.txt")))

(deftest test-part-two
  (testing "Undo file"
    (is (= part-one-initial (undo-file "dbfgaehc" "input.txt")))))


(deftest test-undo-rot-based
  (testing "Undo rot based"
    (is (= "gaebcdhf" (exec-rot-based "dhfgaebc" "rotate based on position of letter e" true)))))


