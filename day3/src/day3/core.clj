(ns day3.core
  (:gen-class))
(require '[clojure.string :as str])

(defn is-triangle
  [[x y z]]
  (let [a (> (+ x y) z)
       b (> (+ y z) x)
       c (> (+ z x) y)]
    (and a b c)))

(defn count-triangles
  [li]
  (count (filter is-triangle li)))

(defn read-input-file
  []
  (slurp "input.txt"))

(defn parse-line
  [line]
  (map #(Integer/parseInt %) (str/split (str/trim line) #"\s+")))

(defn parse-input
  [raw-input]
  (map parse-line (str/split raw-input #"\n")))

(defn get-input
  []
  (parse-input (read-input-file)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (count-triangles (get-input)))

