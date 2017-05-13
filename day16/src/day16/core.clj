(ns day16.core
  (:gen-class)
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io])
  (:import (org.apache.commons.io.input ReversedLinesFileReader)))


(defn copy-file
  [src dest]
  (io/copy (io/file src) (io/file dest)))

(defn append-file
  [dest string]
  (spit dest string :append true))

(defn swap-one
  [c]
  (if (= \1 c) \0 \1))

(defn swap
  [data]
  (map #(swap-one %) (seq data)))

(defn reverse-append-file
  [src dest]
  (let [reader (ReversedLinesFileReader. (io/file src))]
    (loop [line (reverse (.readLine reader))]
      (if line 
        (do 
          (spit dest (str/join (swap line)) :append true)
          (spit dest \newline :append true)
          (recur (.readLine reader)))
        dest 
        ))))

(defn transform
  [data]
  (let [rev (swap (reverse data))
        l (concat (lazy-seq data) [\0] )]
  (concat l rev)))

(defn generate-data
  [data n]
  (if (>= (count data) n) data
      (recur (transform data) n)))

(defn transform-file
  [src-id]
  (let [dest-id (inc src-id)
        src (str "data/" src-id)
        dest (str "data/" dest-id)]
    (do
      (copy-file src dest)
      (append-file dest \0)
      (append-file dest \newline)
      (reverse-append-file src dest)) 
    dest-id))

(defn generate-data-file
  [src-id src-size max-size]
  (if (>= src-size max-size) src-id
      (do 
        (println (str "previous file size: " src-size))
        (recur (transform-file src-id) (+ 1 (* 2 src-size)) max-size))))

(defn transform-pair
  [pair]
  (if (= (first pair) (second pair)) \1 \0))

(defn checksum
  [data n]
  (let [pairs (partition 2 (take n data))
        transformed (map transform-pair pairs)]
    (if (even? (count transformed)) 
      (recur transformed n)
      transformed)))

(defn read-char
  [reader]
  (let [c-raw (.read reader)]
    (if (= -1 c-raw) nil
      (let [c (char c-raw)]
        (if (not (= \newline c))
          c
          (recur reader))))))

(defn read-pair
  [reader]
  (let [a (read-char reader)]
    (if a
      (let [b (read-char reader)]
        (if b (str a b))))))

(defn make-file-checksum
  ([src-file dest-file max-size]
  (spit dest-file "")
  (let [reader (io/reader src-file)]
    (loop [pair (read-pair reader)
           checksum-size 0]
      (if (or (not pair) (>= (* 2 checksum-size) max-size )) checksum-size
          (let [output (transform-pair pair)
                new-size (inc checksum-size)]
            (append-file dest-file output)
            (recur (read-pair reader) new-size)))))))

; "data/"data-file-id
(defn file-checksum
  ([data-file-id max-size] (file-checksum data-file-id max-size (str "data/" data-file-id) 0 0))
  ([data-file-id max-size src-file checksum-id checksum-size]
   (println (str "previous checksum size: " checksum-size))
  (let [dest-file (str "data/"data-file-id ".checksum" checksum-id)
        new-checksum-size (make-file-checksum src-file dest-file max-size)]
        (if (or (> checksum-id 50000) (and (> checksum-size 0) (not (even? new-checksum-size))))
            (slurp dest-file) ; return
            (recur data-file-id max-size dest-file (inc checksum-id) new-checksum-size)))))

(defn one-per-line
  [string]
  (str (apply str (interpose \newline string)) \newline))

(defn fill-large-disk
  [initial max-size]
  (spit "data/0" (one-per-line initial))
  (let [data-file-id (generate-data-file 0 (count initial) max-size)
        checksum-src (str "data/" data-file-id)]
        (file-checksum data-file-id max-size)))

(defn fill-disk
  [initial n]
  (let [data (generate-data initial n)]
    (str/join (checksum data n))))

(def puzzle-input "10111011111001111")

(defn part-one
  []
  (fill-disk puzzle-input 272))

(defn part-two
  []
  (fill-large-disk puzzle-input 35651584))


