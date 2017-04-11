(ns day4.core
  (:gen-class))
(require '[clojure.string :as str])


(defn read-file
  []
  (slurp "input.txt"))

(defn get-lines
  []
  (str/split (read-file) #"\n"))

(defn test-line
  []
  (first (get-lines)))

(defn match-letters
  [string]
  (re-seq #"[A-Za-z\-]+" string))

(defn match-digits
  [string]
  (re-seq #"[\d]+" string))

(defn remove-dashes
  [string]
  (str/replace string #"\-" ""))

(defn parse-line
  [line]
  (def letters (remove-dashes (first (match-letters line))))
  (def checksum (second (match-letters line)))
  (def sector-id (Integer/parseInt (first (match-digits line))))
  {:letters letters :sector-id sector-id :checksum checksum})

(defn my-sort
  [m]
   (sort-by (fn [x] [(- (get m x)) x]) (keys m)))

(defn make-checksum
  [x]
  (def f (frequencies (:letters x)))
  (str/join (take 5 (my-sort f))))

(defn sum-if-valid
  [line]
  (def p (parse-line line))
  (def c (make-checksum p))
  (if (= c (:checksum p))
    (:sector-id p)
    0))

(defn sum-ids
  [lines]
  (if (empty? lines)
    0
    (let [s (sum-if-valid (first lines))]
     (+ s (sum-ids (rest lines))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
