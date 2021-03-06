(ns day24.path-finding
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer :all]))
(use '[clojure.pprint :only (pprint)])

(defn abs
  [x]
  (if (> x 0) x (* x -1)))

(def start-state [0 0])

(def neigh-offsets
  [[-1  0],
   [1   0],
   [0   1],
   [0  -1]])

(defn add-states
  [a b]
  (map + a b))

(defn sub-states
  [a b]
  (map - a b))

(defn in-matrix?
  [matrix x]
  (let [i (first x)
        j (second x)
        maxi (count matrix)
        maxj (count (first matrix))]
    (and
      (>= i 0)
      (>= j 0)
      (< i maxi)
      (- j maxj))))

(defn free?
  [matrix pos]
  (let [x (first pos)
        y (second pos)
        ch (nth (nth matrix x) y)]
    (not (= "#" ch))))

(defn get-neighbors
  [matrix closed pos]
  (let [neighbors (filter #(free? matrix %) (filter #(in-matrix? matrix %) (map #(add-states pos %) neigh-offsets)))]
    (filter #(not (contains? closed %)) neighbors)))

(defn get-dist
  [dists k default]
   (let [dist (get dists k)]
     (if dist dist default)))

(defn should-terminate?
  [open dists goal]
      (= 0 (count open)))

(defn find-by-pos
  [m v]
  (first (first (filter #(= (:pos (first %)) (:pos v)) m))))

(defn check-neighbors
  [heuristic open closed nn fscores dists prevs curr goal comp-func default-dist]
    (if (= 0 (count nn))
      [open closed fscores dists prevs]
      (let [v (first nn)
            tentative (+ 1 (get dists curr))
            dist-v (get-dist dists v default-dist)]
        (if (comp tentative dist-v)
            (recur heuristic
                   (assoc open v (+ tentative (heuristic v goal)))
                   closed
                   (rest nn)
                   (assoc fscores v (+ tentative (heuristic v goal)))
                   (assoc dists v tentative)
                   (assoc prevs v curr)
                   curr
                   goal
                   comp-func
                   default-dist)
          (recur heuristic
                 open
                 closed
                 (rest nn)
                 fscores
                 dists
                 prevs
                 curr
                 goal
                 comp-func
                 default-dist)))))

(defn shortest-path
  [heuristic src goal matrix]
   (loop [open (priority-map src (heuristic src goal))
          closed #{}
          fscores (priority-map src (heuristic src goal))
          dists (priority-map src 0 goal Integer/MAX_VALUE)
          prevs {}]
     (if (should-terminate? open dists goal)
       dists
       (let [curr (first (peek open))
             res (check-neighbors heuristic
                                  (pop open)
                                  (conj closed curr)
                                  (get-neighbors matrix closed curr)
                                  fscores
                                  dists
                                  prevs
                                  curr
                                  goal
                                  (fn [a b] (< a b))
                                  Integer/MAX_VALUE) ]
         (recur (get res 0)
                (get res 1)
                (dissoc (get res 2) curr) ; remove curr from fscores
                (get res 3)
                (get res 4))))))

(defn parse-line
  [line]
  (str/split line #""))

(defn get-lines
  [txt]
  (str/split txt #"\n"))

(defn read-input
  [file-name]
  (map parse-line (get-lines (slurp file-name))))

(defn heuristic1
  [a b]
  (reduce + (map abs (sub-states b a))))

(defn find-distances
  [file-name]
  (read-input file-name))

(defn str-numbers
 [n] (map str (range 0 n)))

(defn find-in-array
  [array nbr]
  (.indexOf array nbr))

(defn find-nonzero
 ([array] (find-nonzero array 0))
 ([array acc]
  (if (= (count array) 0) -1
    (if (>= (first array) 0)
      [acc (first array)]
      (find-nonzero (rest array) (+ acc 1))))))

(defn find-in-matrix
  [input nbr]
  (let [find-results (map #(find-in-array % nbr) input)]
    (find-nonzero find-results)))

; Find the position of the numbers in the given input matrix
; n is the amount of numbers
(defn get-targets
  [input n]
  (map #(find-in-matrix input %) (str-numbers n)))

(defn distance-between
  [input start goal]
  (shortest-path heuristic1 start goal input))

(defn simple-distance-between
  [input start goal]
  (get (shortest-path heuristic1 start goal input) goal))

(defn list-contains
  [ll x]
  (some #(= x %) ll))

(defn filter-dists-map
  "remove distance to non-targets and start to itself"
  [dists-map start targets]
  (let [without-start (dissoc dists-map start)]
        (filter #(list-contains targets (first %))) without-start))

(defn get-all-dists
  "Calculates every distance between the points"
  ([input start-i targets] (get-all-dists input start-i targets {}))
  ([input start-i targets all-dists-map]
   (if (= (count targets) start-i) all-dists-map
   (let [start (nth targets start-i)
         others (remove #(= start %) targets)
         dists-map (distance-between input start (first others))
         new-all-dists-map (assoc all-dists-map start dists-map)]
    (recur input (inc start-i) targets new-all-dists-map)))))

(def input1 (read-input "input.txt"))
(def tinput (read-input "test-input.txt"))

(defn get-part1-targets
  []
  (get-targets input1 8))

(defn get-part1-dists
  []
  (get-all-dists input1 0 (get-part1-targets)))

(defn get-test-targets
  []
  (get-targets tinput 5))

(defn get-test-dists
  []
  (get-all-dists tinput 0 (get-test-targets)))



