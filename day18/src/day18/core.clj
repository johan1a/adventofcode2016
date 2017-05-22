(ns day18.core
  (:gen-class)
  (:require [clojure.string :as str]))


(def input ".^.^..^......^^^^^...^^^...^...^....^^.^...^.^^^^....^...^^.^^^...^^^^.^^.^.^^..^.^^^..^^^^^^.^^^..^")

(def trap-patterns
  #{ [\^ \^ \.]
     [\. \^ \^]
     [\^ \. \.]
     [\. \. \^]})

(defn next-pos
  [prev-row next-row-index]
  (let [prev-row-index (+ 1 next-row-index)
        tiles-behind (take 3 (drop next-row-index prev-row))]
   (if (contains? trap-patterns tiles-behind) 
       \^
       \.)))

(defn padded-row
  [row]
  (str \. row \.))

(defn next-row
  [row]
  (str/join (map #(next-pos (padded-row row) %) (range (count row)))))

(defn make-map1
  [previous n]
  (if (= 1 n) 
    previous
    (recur (conj previous (next-row (last previous))) (dec n))))

(defn make-map
  [initial-row num-rows]
  (make-map1 [initial-row] num-rows))

(defn count-safe-row
  [row]
  (count (filter #(= \. %) row)))

(defn count-safe
  [trap-map]
  (reduce + (map count-safe-row trap-map)))

(defn part-one
  []
  (count-safe (make-map input 40)))

(defn test-one
  []
  (count-safe (make-map ".^^.^.^^^^" 10)))

(defn count-map
  ([initial-row row-count] (count-map initial-row (count-safe [initial-row]) row-count))
  ([prev-row total-nbr-safe row-count]
  (if (= 0 (mod row-count 1000)) (pprint row-count))
  (if (= 1 row-count) 
      total-nbr-safe
      (let [row (next-row prev-row)
            row-nbr-safe (count-safe [row])]
        (recur row (+ total-nbr-safe row-nbr-safe) (dec row-count))))))

(defn part-two
  []
  (count-map input 400000))


