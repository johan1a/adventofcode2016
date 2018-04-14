(ns day24.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer :all]
            [clojure.math.combinatorics :as combo]
            [day24.path-finding :as pf]))
(use '[clojure.pprint :only (pprint)])

(defn all-permutations
  [positions]
  (combo/permutations positions))

(defn get-dist
  [dists start next-pos]
  (let [dist-map (get dists start)]
    (get dist-map next-pos)))

(defn calc-distance
  ([dists path] (calc-distance dists path 0))
  ([dists path total]
   (if (= 1 (count path)) total
     (let [start (first path)
           next-pos (second path)
           next-dist (get-dist dists start next-pos)]
     (recur dists (rest path) (+ total next-dist))))))

(defn get-path
  [dists start others]
  (let [path (concat [start] others)
        dist (calc-distance dists path)]
    {:dist dist :path path}))

(defn all-possible-paths
  [dists targets]
  (let [start (first targets)
        others (rest targets)
        permutations (all-permutations others)
        all-paths (map #(get-path dists start %) permutations)]
    all-paths))

(defn solve
  [dists targets]
  (let [all-paths (all-possible-paths dists targets)]
    (apply min (map :dist all-paths))))

(defn part-one
  []
  (solve (pf/get-part1-dists) (pf/get-part1-targets)))

(defn test-solve
  []
  (solve (pf/get-test-dists) (pf/get-test-targets)))
"30 2 -> 3"



