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


(defn decompress
  [ss]
  (let [matches (re-find #"(\d+)x(\d+)\)" ss )
        a (Integer/parseInt (nth matches 1))
        b (Integer/parseInt (nth matches 2))
        skip-length (count (str a "x" b ")"))
        after-skipped  (subs ss skip-length)
        to-repeat (subs after-skipped 0 a)
        repetition (str/join (repeat b to-repeat))
        after-repetition (subs after-skipped a)
        new-ss (str repetition after-repetition)]
    new-ss))

(defn count-length
  [ss acc]
  (if (= 0 (count ss))
    acc 
    (let [f (first ss)
          res (subs ss 1)]
     (if (= \( f)
      (let [new-ss (decompress res)]
        (recur new-ss acc))
      (recur res (+ acc 1))))))

(defn part-one
  []
  (count (parse (read-file "input.txt"))))



(defn make-new-weights
  [weights curr a b]
  (let [multiplied (map #(* b %) (take a (drop curr weights)))
        before (take curr weights)
        after (drop (+ curr a) weights)]
    (concat before multiplied after)))

(defn mul-weights
  [ss weights curr]
  (let [res (subs ss curr)
        matches (re-find #"\((\d+)x(\d+)\)" res )
        a (Integer/parseInt (nth matches 1))
        b (Integer/parseInt (nth matches 2))
        skip-length (count (nth matches 0))
        new-curr (+ curr skip-length)
        new-weights (make-new-weights weights new-curr a b)]
    [new-weights "" new-curr]))

(defn count-2 
  ([ss] (count-2 ss (repeat (count ss) 1) 0 0))
  ([ss weights len curr]
    (if (= curr (count ss))
      len
      (let [f (nth ss curr)
          res (subs ss 1)]
       (if (= \( f)
        (let [d (mul-weights ss weights curr)
              new-weights (first d)
              new-curr (last d)]
          (recur ss new-weights len new-curr))
        (let [new-len (+ len (nth weights curr))]
          (recur ss weights new-len (inc curr))))))))




(defn part-two
  []
  (count-2 (read-file "input.txt")))


