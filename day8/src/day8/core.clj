(ns day8.core
  (:gen-class))
(require '[clojure.string :as str])
(use 'clojure.core.matrix)

(defn read-file
  [file-name]
  (map str/trim (str/split (slurp file-name) #"\n")))

(defn get-col
  [s i]
  (map #(nth % i) s))

(defn put-col
  [s col-i c]
  (vec (map-indexed (fn [row-i row] (assoc row col-i (nth c row-i))) s)))

(defn mk-vect
  [n v]
  (vec (repeat n v)))

(defn pretty
  [s]
  (map println s))

(defn make-screen
  [rows cols ]
  (mk-vect cols (mk-vect rows 0)))

(def empty-state
  (make-screen 50 6))

(defn turn-on-row
  [s i n]
  (let [old-row (nth s i)
        active-part (mk-vect n 1)
        part-after (drop n old-row)
        new-row (apply conj active-part part-after)]
   (assoc s i new-row)))

(defn rect
  [s nbr-cols nbr-rows ]
  (if (= 0 nbr-rows)
    s
    (let [new-s (turn-on-row s (- nbr-rows 1) nbr-cols)]
     (recur new-s nbr-cols (dec nbr-rows)))))

(defn rotate-row
  [s row-i n]
  (let [r (nth s row-i)
        rotated (rotate r 0 (- n))]
  (assoc s row-i rotated)))

(defn rotate-col
  [s col-i n]
  (let [c (get-col s col-i)
        rotated (rotate c 0 (- n))]
    (put-col s col-i rotated)))

(defn exec-rect 
  [s string]
  (let [matches (re-matches #"rect (\d+)x(\d+)" string)
        a (Integer/parseInt (nth matches 1))
        b (Integer/parseInt (nth matches 2))] 
        (rect s a b)))

(defn exec-rotate-col
  [s string]
  (let [matches (re-matches #"rotate column x=(\d+) by (\d+)" string)
        a (Integer/parseInt (nth matches 1))
        b (Integer/parseInt (nth matches 2))]
        (rotate-col s a b)))

(defn exec-rotate-row
  [s string]
  (let [matches (re-matches #"rotate row y=(\d+) by (\d+)" string)
        a (Integer/parseInt (nth matches 1))
        b (Integer/parseInt (nth matches 2))] 
        (rotate-row s a b)))

(defn exec-line
  [s line]
  (if (str/starts-with? line "rect")
    (exec-rect s line )
    (if (str/starts-with? line "rotate row")
      (exec-rotate-row s line)
      (exec-rotate-col s line))))

(defn exec-lines
  [s lines]
  (if (= 0 (count lines))
  s
  (let [new-state (exec-line s (first lines))]
   (exec-lines new-state (rest lines)))))

(defn sum
  [s]
  (reduce + (map #(reduce + %) s)))

(defn exec-file
  [file-name]
  (let [lines (read-file file-name)
        final-state (exec-lines empty-state lines)]
  (sum final-state)))


(defn part-one
  []
  (exec-file "input.txt"))





