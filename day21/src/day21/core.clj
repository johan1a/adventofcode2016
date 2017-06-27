(ns day21.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def test-initial "abcde")
(def part-one-initial "abcdefgh")
(def part-two-initial "fbgdceah")

(defn read-file
  [file-name]
  (map str/trim (str/split (slurp file-name) #"\n")))

(defn to-char
  [s]
  (first s))

(defn swap-pos
  "the letters at indexes X and Y (counting from 0) should be swapped."
  [s x y]
  (if (> x y) (swap-pos s y x)
    (let [letter-x (nth s x)
          letter-y (nth s y) 
          head (subs s 0 x)
          middle (subs s (inc x) y)
          tail (subs s (inc y))]
      (str head letter-y middle letter-x tail))))

(defn swap-letter
  "the letters X and Y should be swapped (regardless of where they appear in the string)"
  [s x y]
  (apply str (map #(cond
          (= x %) y
          (= y %) x
          :else %)
            s)))

(defn rot-left
  [s x]
  (let [x-mod (mod x (count s))]
    (apply str (concat (drop x-mod s) (take x-mod s)))))

(defn rot-right
  [s x]
  (let [x-mod (mod x (count s))]
    (apply str (concat (drop (- (count s) x-mod) s) (take (- (count s) x-mod) s)))))

(defn rot-based
  "the whole string should be rotated to the right 
  based on the index of letter X. Once the index is determined, rotate the string to the right one time, plus a number of times equal to that index, plus one additional time if the index was at least 4."
  [s x]
  (let [index (str/index-of s x)  
        bonus (if (>= index 4) 1 0)
        rot-count (+ 1 index bonus)]
  (rot-right s rot-count)))

(defn reverse-pos
  "letters at indexes X through Y (including the letters at X and Y) should be reversed in order"
  [s x y]
  (let [first-part (subs s 0 x)
        middle-part (subs s x (inc y))
        after-part (subs s (inc y))
        reversed-middle (str/reverse middle-part)]
  (apply str (concat first-part reversed-middle after-part))))

(defn move-pos
  "the letter which is at index X should be removed from the string, then inserted such that it ends up at index Y"
  [s x y]
  (let [letter (nth s x)
        removed-x (str/replace-first s letter "")]
  (apply str (concat (take y removed-x) [letter] (drop y removed-x)))))

(defn undo-swap-pos
  [s x y]
  (swap-pos s x y))

(defn undo-swap-letter
  [s x y]
  (swap-letter s x y))

(defn undo-rot-left
  [s x]
  (rot-right s x))

(defn undo-rot-right
  [s x]
  (rot-left s x))

(def inverse-rot-based [9 1 6 2 7 3 8 4])

(defn undo-rot-based
  [s x]
  (let [index (str/index-of s x)  
        rot-count (nth inverse-rot-based index)]
  (rot-left s rot-count)))

(defn undo-reverse-pos
  [s x y]
  (reverse-pos s x y))

(defn undo-move-pos
  [s x y]
  (move-pos s y x))

(defn exec-swap-pos
  ([s string] (exec-swap-pos s string false))
  ([s string undo]
  (let [matches (re-matches #"swap position (\d+) with position (\d+)" string)
        x (Integer/parseInt (nth matches 1))
        y (Integer/parseInt (nth matches 2))] 
        (if undo (undo-swap-pos s x y) (swap-pos s x y)))))

(defn exec-swap-letter
  ([s string] (exec-swap-letter s string false))
  ([s string undo]   
    (let [matches (re-matches #"swap letter (.) with letter (.)" string)
        x (to-char (nth matches 1))
        y (to-char (nth matches 2))]
        (if undo (undo-swap-letter s x y) (swap-letter s x y)))))

(defn exec-rot-left
  ([s string] (exec-rot-left s string false))
  ([s string undo]
  (let [matches (re-matches #"rotate left (\d+) \w+" string)
        x (Integer/parseInt (nth matches 1)) ] 
        (if undo (undo-rot-left s x) (rot-left s x)))))

(defn exec-rot-right
  ([s string] (exec-rot-right s string false))
  ([s string undo]
    (let [matches (re-matches #"rotate right (\d+) \w+" string)
        x (Integer/parseInt (nth matches 1)) ] 
        (if undo (undo-rot-right s x) (rot-right s x)))))

(defn exec-rot-based
  ([s string] (exec-rot-based s string false))
  ([s string undo]
  (let [matches (re-matches #"rotate based on position of letter (.)" string)
        x (to-char (nth matches 1)) ] 
        (if undo (undo-rot-based s x) (rot-based s x)))))

(defn exec-reverse-pos
  ([s string] (exec-reverse-pos s string false))
  ([s string undo]
  (let [matches (re-matches #"reverse positions (\d+) through (\d+)" string)
        x (Integer/parseInt (nth matches 1))
        y (Integer/parseInt (nth matches 2))] 
        (if undo (undo-reverse-pos s x y) (reverse-pos s x y)))))

(defn exec-move-pos
  ([s string] (exec-move-pos s string false))
  ([s string undo]
  (let [matches (re-matches #"move position (\d+) to position (\d+)" string)
        x (Integer/parseInt (nth matches 1))
        y (Integer/parseInt (nth matches 2))] 
        (if undo (undo-move-pos s x y) (move-pos s x y)))))

(defn exec-line
  ([s line] (exec-line s line false))
  ([s line undo]
; (if undo 
;     (println (str "[" s "] next: undoing " line))
;     (println (str "[" s "] next: " line)))
  (cond 
    (str/starts-with? line "swap position") (exec-swap-pos s line undo)
    (str/starts-with? line "swap letter") (exec-swap-letter s line undo)
    (str/starts-with? line "rotate left") (exec-rot-left s line undo)
    (str/starts-with? line "rotate right") (exec-rot-right s line undo)
    (str/starts-with? line "rotate based") (exec-rot-based s line undo)
    (str/starts-with? line "reverse positions") (exec-reverse-pos s line undo)
    (str/starts-with? line "move position") (exec-move-pos s line undo))))

(defn exec-lines
  ([s lines] (exec-lines s lines false))
  ([s lines undo]
  (if (= 0 (count lines))
  s
  (let [new-state (exec-line s (first lines) undo)]
   (exec-lines new-state (rest lines) undo)))))

(defn exec-file
  [initial file-name]
  (let [lines (read-file file-name)]
    (exec-lines initial lines)))

(defn undo-file
  [initial file-name]
  (let [lines (reverse (read-file file-name))
        should-undo true]
    (exec-lines initial lines should-undo)))

(defn part-one
  []
  (exec-file part-one-initial "input.txt"))

(defn part-two
  []
  (undo-file part-two-initial "input.txt"))

(defn -main
  []
  (part-one))




