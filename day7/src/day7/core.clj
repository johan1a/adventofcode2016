(ns day7.core
  (:gen-class))
(require '[clojure.string :as str])

(defn read-file
  [file-name]
  (map str/trim (str/split (slurp file-name) #"\n")))

(def ip-pattern #"(.+)\[(.*)\](.+)")

(def hypernet-sequence #"\[[a-zA-Z]*\]")

(defn get-inside
  [line]
  (let [with-brackets (re-seq #"\[[a-zA-Z]*\]" line)]
   (map #(str/replace % #"[\[\]]" "") with-brackets)))

(defn parse-line
  [line]
  (let [out (str/split line hypernet-sequence)
        in (get-inside line)]
    {:inside in :outside out}))

(defn is-palindrome
  [string]
  (and 
    (= string (str/reverse string))
    (> (count (distinct string)) 1)))

(defn all-subs
  [s]
  (let [length (+ (count s) 1)]
  (for [i (range 0 length)
        j (range (+ i 1) length)]
    (subs s i j))))

(defn valid-subs
   [ss]
  (filter #(= 4 (count %)) (all-subs ss)))

(defn contains-palindrome
  [string]
  (> (count (filter is-palindrome (valid-subs string))) 0 ))

(defn with-palindromes
  [ss]
  (count (filter contains-palindrome ss)) )

(defn is-valid
  [x]
  (and
  (> (with-palindromes (:outside x)) 0 )
  (= (with-palindromes (:inside x)) 0 )))

(defn count-valid
  [lines]
    (count (filter is-valid lines)))

(defn get-lines
  [file-name]
  (map parse-line (read-file file-name)))


(defn count-in-file
  [file-name]
  (count-valid (get-lines file-name)))

(defn part-one
  []
  (count-in-file "input.txt"))
