(ns day6.core
  (:gen-class))
(require '[clojure.string :as str])

(defn read-file
  [file-name]
  (map str/trim (str/split (slurp file-name) #"\n")))

(defn test-input
  []
  (read-file "testinput.txt"))

(defn parse-line
  [line]
  (map-indexed (fn [i a] [i a]) line))

(defn columns
  [msgs]
  (partition (count msgs) (apply interleave msgs)))

(defn make-input
  [lines]
  (map parse-line lines))

(defn get-frequencies
  [msgs]
  (map #(sort-by second %) (map frequencies (columns msgs))))

(defn error-correct
  [msgs f]
  (str/join (map first (map f (get-frequencies msgs)))))

(defn test-part1
  []
  (error-correct (test-input) last))

(defn part1
  []
  (error-correct (read-file "input.txt") last))

(defn test-part2
  []
  (error-correct (read-file "testinput.txt") first))

(defn part2
  []
  (error-correct (read-file "input.txt") first))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
