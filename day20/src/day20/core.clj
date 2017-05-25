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


(def max-val 4294967295)

(defn count-free
  ([ranges] (count-free ranges 0))
  ([ranges sum]
   (if (= 1 (count ranges)) sum
     (let [a (first ranges)
           b (second ranges)
           gap (- (first b) (second a))]
       (recur (rest ranges) (+ sum gap))))))

(defn find-ranges
  ([input] (find-ranges [[-1 -1]] input))
  ([ranges input]
  (if (empty? input) (count-free (reverse ranges))
    (let [[low high] (first ranges)
          [input-low input-high] (first input)]
      (if (> input-low (+ 1 high)) (recur (cons [input-low input-high] ranges) (rest input))
        (recur (cons [low (max high input-high)] (rest ranges)) (rest input)))))))
