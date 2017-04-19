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
   [ss n]
  (filter #(= n (count %)) (all-subs ss)))

(defn get-palindromes
  [string n]
  (filter is-palindrome (valid-subs string n)))

(defn contains-palindrome
  [string n]
  (> (count (get-palindromes string n)) 0 ))

(defn with-palindromes
  [ss]
  (count (filter #(contains-palindrome % 4) ss )) )

(defn is-valid
  [x]
  (and
  (> (with-palindromes (:outside x)) 0 )
  (= (with-palindromes (:inside x)) 0 )))

(defn get-abas
  [ss]
  (flatten (map #(get-palindromes % 3) ss)))

(defn reverse-aba
  [aba]
  (str (second aba) (first aba) (second aba)))

;;http://stackoverflow.com/questions/16264813/clojure-idiomatic-way-to-call-contains-on-a-lazy-sequence
(defn lazy-contains? [coll key]
    (boolean (some #(= % key) coll)))

(defn is-valid-ssl
  [x]
  (let [abas ( get-abas (:outside x))
        needed-babs (map reverse-aba abas)
        found-babs (get-abas (:inside x))]
  (> (count (filter #(lazy-contains? needed-babs %) found-babs)) 0)))

(defn count-valid
  [f lines]
    (count (filter f lines)))

(defn get-lines
  [file-name]
  (map parse-line (read-file file-name)))

(defn count-in-file
  [f file-name]
  (count-valid f (get-lines file-name)))

(defn part-one
  []
  (count-in-file is-valid "input.txt"))

(defn part-two
  []
  (count-in-file is-valid-ssl "input.txt" ))

(defn test-part-two
  []
  (count-in-file is-valid-ssl "test-input2.txt" ))








