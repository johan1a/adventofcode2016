(ns day21.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def test-initial"abcde")
(def initial "abcdefgh")

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

(defn exec-swap-pos
  [s string]
  (let [matches (re-matches #"swap position (\d+) with position (\d+)" string)
        x (Integer/parseInt (nth matches 1))
        y (Integer/parseInt (nth matches 2))] 
        (swap-pos s x y)))

(defn exec-swap-letter
  [s string]
  (let [matches (re-matches #"swap letter (.) with letter (.)" string)
        x (to-char (nth matches 1))
        y (to-char (nth matches 2))]
        (swap-letter s x y)))

(defn exec-rot-left
  [s string]
  (let [matches (re-matches #"rotate left (\d+) \w+" string)
        x (Integer/parseInt (nth matches 1)) ] 
        (rot-left s x)))

(defn exec-rot-right
  [s string]
  (let [matches (re-matches #"rotate right (\d+) \w+" string)
        x (Integer/parseInt (nth matches 1)) ] 
        (rot-right s x)))

(defn exec-rot-based
  [s string]
  (let [matches (re-matches #"rotate based on position of letter (.)" string)
        x (to-char (nth matches 1)) ] 
        (rot-based s x)))

(defn exec-reverse-pos
  [s string]
  (let [matches (re-matches #"reverse positions (\d+) through (\d+)" string)
        x (Integer/parseInt (nth matches 1))
        y (Integer/parseInt (nth matches 2))] 
        (reverse-pos s x y)))

(defn exec-move-pos
  [s string]
  (let [matches (re-matches #"move position (\d+) to position (\d+)" string)
        x (Integer/parseInt (nth matches 1))
        y (Integer/parseInt (nth matches 2))] 
        (move-pos s x y)))

(defn exec-line
  [s line]
	(pprint (str "[" s "] next: " line))
  (cond 
    (str/starts-with? line "swap position") (exec-swap-pos s line)
    (str/starts-with? line "swap letter") (exec-swap-letter s line)
    (str/starts-with? line "rotate left") (exec-rot-left s line)
    (str/starts-with? line "rotate right") (exec-rot-right s line)
    (str/starts-with? line "rotate based") (exec-rot-based s line)
    (str/starts-with? line "reverse positions") (exec-reverse-pos s line)
    (str/starts-with? line "move position") (exec-move-pos s line)))

(defn exec-lines
  [s lines]
  (if (= 0 (count lines))
  s
  (let [new-state (exec-line s (first lines))]
   (exec-lines new-state (rest lines)))))

(defn exec-file
  [initial file-name]
  (let [lines (read-file file-name)]
    (exec-lines initial lines)))

(defn test-part-one
  []
  (exec-file test-initial "test-input.txt"))

(defn part-one
	[]
	(exec-file initial "input.txt"))




