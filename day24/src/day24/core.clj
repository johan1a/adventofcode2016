(ns day24.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer :all]))
(use '[clojure.pprint :only (pprint)])

(defn abs
  [x]
  (if (> x 0) x (* x -1)))

(defn neighbors
  ([state] (neighbors state #{}))
  ([state closed]
   "todo"))

;;#--"Every dist should be 1"
(defn get-dist
  [dists k default]
  1)

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
  [heuristic src goal]
   (loop [open (priority-map src (heuristic src goal))
          closed #{}
          fscores (priority-map src (heuristic src goal))
          dists (priority-map src 0 goal Integer/MAX_VALUE)
          prevs {}]
     (if (should-terminate? open dists goal)
       (find-by-pos open goal)
       (let [curr (first (peek open))
             res (check-neighbors heuristic
                                  (pop open)
                                  (conj closed curr)
                                  (neighbors curr closed)
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
  (reduce + (map abs ( b a))))

(defn solve
  [start-state]
  (subs (:path (shortest-path heuristic1 start-state nil)) (count (:path start-state))))

(def start "TODO")

(defn part-one
  []
  (solve start))

