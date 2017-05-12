(ns day16.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn swap-one
  [c]
  (if (= \1 c) \0 \1))

(defn swap
  [data]
  (map #(swap-one %) (seq data)))

(defn transform
  [data]
  (let [rev (swap (reverse data))
        l (concat (seq data) [\0] )]
  (concat l rev)))

(defn generate-data
  [data n]
  (if (>= (count data) n) data
      (recur (transform data) n)))

(defn transform-pair
  [pair]
  (if (= (first pair) (second pair)) \1 \0))

(defn checksum
  [data n]
  (let [pairs (partition 2 (take n data))
        transformed (map transform-pair pairs)]
    (if (even? (count transformed)) 
      (recur transformed n)
      transformed)))

(defn fill-disk
  [initial n]
  (let [data (generate-data initial n)]
    (str/join (checksum data n))))

(def puzzle-input "10111011111001111")

(defn part-one
  []
  (fill-disk puzzle-input 272))
