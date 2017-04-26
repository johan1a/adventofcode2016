(ns day9.core
  (:gen-class))
  (require '[clojure.string :as str])

(defn read-file
  [file-name]
  (str/replace (str/trim (slurp file-name)) #"\s" ""))

(defn do-repeat
  [ss]
  (let [matches (re-find #"(\d+)x(\d+)\)" ss )
        a (Integer/parseInt (nth matches 1))
        b (Integer/parseInt (nth matches 2))
        skip-length (count (str a "x" b ")"))
        skipped (str/join (drop skip-length ss))
        to-repeat (str/join (take a skipped))
        repetition (str/join (repeat b to-repeat))
        after-repetition (str/join (drop a skipped))]
    [repetition after-repetition]))

(defn parse
  [ss]
  (if (= 0 (count ss))
    ""
    (let [f (first ss)
          res (str/join (rest ss))]
     (if (= \( f)
      (let [d (do-repeat res)
           repetition (first d)
           res2 (second d)]
        (str repetition (parse res2)))
      (str f (parse res))))))

(defn part-one
  []
  (count (parse (read-file "input.txt"))))


