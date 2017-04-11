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

(def alphabet (seq "abcdefghijklmnopqrstuvwxyz"))

(def alphabet-size (count alphabet))

(defn alpha-index
  [ch]
  (.indexOf alphabet ch))

(defn total-offset
  [ch n]
  (mod (+ (alpha-index ch) n) alphabet-size))

(defn shift-char
  [ch n]
  (if (= ch \-)
    \space
    (nth alphabet (total-offset ch n))))

(defn shift-string
  [s n]
  (str/join (map #(shift-char % n) s)))

(defn decode
  [x]
  (shift-string (:letters x) (:sector-id x)))

(defn make-decoded
  [line]
  (let [x (parse-line line)]
  {(decode x) (:sector-id x)}))

(defn decode-lines
  [lines]
  (if (empty? lines)
    {}
    (merge (make-decoded (first lines)) (decode-lines (rest lines)))))

(defn find-north-pole-objects
  []
  (filter #(.contains (first %) "north") (decode-lines (get-lines))))




































