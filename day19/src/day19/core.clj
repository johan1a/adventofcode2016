(ns day19.core
  (:gen-class)
  (:require [clojure.string :as str]))


(defn new-gnome
  [k]
  {:number k :present-count 1})

(defn make-gnomes
  [n]
  (map new-gnome (range 1 (+ 1 n))))

(defn take-from
  [gnome1 gnome2]
  (let [number (:number gnome1)
        present-count (+ (:present-count gnome1) (:present-count gnome2))]
    {:number number :present-count present-count}))

(defn drop-nth
  [ll n]
  (concat (take n ll) 
          (drop (+ 1 n) ll)))

(defn insert
  [ll n elem]
  (lazy-cat (take n ll) 
            [elem]
            (drop (+ 1 n) ll)))


(defn take-presents
  [gnomes g-count i]
  (let [i1 (mod i g-count)
        i2 (mod (+ 1 i) g-count)
        gnome1 (nth gnomes i1)
        gnome2 (nth gnomes i2)
        new-gnome1 (take-from gnome1 gnome2)
        new-gnomes1 (insert gnomes i1 new-gnome1)
        new-gnomes2 (drop-nth new-gnomes1 i2)]
   new-gnomes2))

(defn play-game
  ([gnomes] (play-game gnomes (count gnomes) 0))
  ([gnomes g-count i]
   (if (= 0 (mod g-count 1000)) (time (pprint (count gnomes))))
  (if (= 1 g-count) 
    (first gnomes)
    (let [new-index (if (= (- g-count 1) i) 0 (inc i))]
     (recur (take-presents gnomes g-count i) (dec g-count) new-index )))))

(defn part-one
  []
  (play-game (make-gnomes 3001330)))

