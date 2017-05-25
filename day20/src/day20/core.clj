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


(def real-max-val 4294967295)

(defn count-free
  ([max-val ranges] (count-free max-val ranges 0))
  ([max-val ranges sum]
   (if (= 1 (count ranges)) (+ sum (- max-val (second (first ranges))))
     (let [first-high (second (first ranges))
           second-low (first (second ranges))
           upper-limit (min second-low max-val)
           gap (- (- upper-limit first-high) 1)
           new-sum (+ sum gap)]
       (if (> second-low max-val) new-sum
       (recur max-val (rest ranges) new-sum))))))

(defn find-ranges
  ([input] (find-ranges [[-1 -1]] input))
  ([ranges input]
  (if (empty? input) ranges
    (let [[low high] (first ranges)
          [input-low input-high] (first input)]
      (if (> input-low (+ 1 high)) (recur (cons [input-low input-high] ranges) (rest input))
        (recur (cons [low (max high input-high)] (rest ranges)) (rest input)))))))

(defn part-two
  []
 (count-free real-max-val (reverse (find-ranges (get-input)))))

