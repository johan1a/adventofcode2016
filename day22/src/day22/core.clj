(ns day22.core
  (:gen-class)
  (:require [clojure.string :as str]))


(defn parse-num
  [t-num]
  (Integer/parseInt (subs t-num 0 (- (count t-num) 1))))

(defn parse-line
  [line]
  (let [splitted (str/split line #"\s+")
        path (nth splitted 0)
        x (Integer/parseInt (first (str/split (second (str/split path #"/dev/grid/node-x")) #"-y")))
        y (Integer/parseInt (last (str/split path #"-y")))
        size (parse-num (nth splitted 1))
        used (parse-num (nth splitted 2))
        avail (parse-num (nth splitted 3))
        use- (parse-num (nth splitted 4))]
    {:pos [x y] :size size :used used :avail avail :use% use-}))


(defn get-input
  []
  (map parse-line (str/split (slurp "input.txt") #"\n")))

(defn sort-by-
  [nodes attr]
  (sort-by (fn [node] (attr node)) nodes))

(defn is-viable?
  [a b]
  (and (not (= a b))
       (> (:used a) 0)
       (<= (:used a) (:avail b))))
node-neighbors

(defn possible-moves-node
  [nodes node]
  {(:pos node) (map :pos (viable-targets nodes node))}  )

(defn possible-moves
  [nodes]
  (map #(possible-moves-node nodes %) nodes))

(defn viable-targets
  [nodes node]
  (filter #(is-viable? node %) nodes))

(defn all-viable
  [nodes]
   (map #(count (viable-targets nodes %)) nodes))

(defn count-viable
  [nodes]
  (reduce + (all-viable nodes)))

(defn part-one
  []
  (count-viable (get-input)))



