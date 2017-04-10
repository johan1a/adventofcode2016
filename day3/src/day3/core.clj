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

(defn add-edge
  [ll col edge]
  (update ll col #(conj % edge)))

(defn add-lines
  [ll lines]
  (if (empty? lines)
      ll
      (let [line (first lines)
             a (add-edge ll :col0 (nth line 0))
             b (add-edge a  :col1 (nth line 1))
             c (add-edge b  :col2 (nth line 2))]
             (add-lines c (rest lines)))))

(def empty-ll
  {:col0 [] :col1 [] :col2 []})

(defn get-input-lines
  []
  (map parse-line (str/split (read-input-file) #"\n")))

(defn get-input-2
  []
    (def three-cols (add-lines empty-ll (get-input-lines)))
    (def a (:col0 three-cols))
    (def b (:col1 three-cols))
    (def c (:col2 three-cols))
    (partition 3 (concat a b c)))

(defn part-two
  []
  (count-triangles (get-input-2)))


(defn get-input-1
  []
  (get-input-lines))

(defn part-one
  []
  (count-triangles (get-input-1)))

