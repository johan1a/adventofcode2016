(ns day20.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn make-range
  [line]
  (vec (map #(Long/parseLong %) (str/split line #"-"))))


(defn get-input
  []
  (sort (map make-range (str/split (slurp "input.txt") #"\n" ))))

(defn find-lowest
  ([input] (find-lowest -1 -1 input))
  ([low high input]
  (if (empty? input) false
    (let [[input-low input-high] (first input)]
      (if (> input-low (+ 1 high)) (+ 1 high)
        (recur low (max high input-high) (rest input)))))))

(defn part-one
  []
  (find-lowest (get-input)))

