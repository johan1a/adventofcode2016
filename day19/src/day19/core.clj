(ns day19.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn make-gnome
  [k]
   {:number (+ 1 k) :present-count 1})

(defn make-nexts
  ([gnomes k n] (make-nexts gnomes k n {}))
  ([gnomes k n nexts]
    (if (= k n) (assoc nexts k (nth gnomes 0))
      (recur gnomes (inc k) n (assoc nexts k (nth gnomes k))))))

(defn make-gnomes
  [n]
  (let [gnomes (map make-gnome (range n))
        nexts (apply merge (map #(assoc {} (- (:number %) 1) %) gnomes))]
    (dissoc (assoc nexts n (first gnomes)) 0)))

(def make-gnomes-m (memoize make-gnomes))

(defn update-presents
  [nexts prev-n n new-count]
  (let [next- (get nexts n)
        next-next (get nexts (:number next-))
        updated-n (assoc nexts prev-n {:number n :present-count new-count})
        updated-n-next (assoc updated-n n next-next) ]
    (if (= (:number next-next) n)
        (dissoc updated-n-next n)
        (dissoc updated-n-next (:number next-)))))

(defn take-presents
  [nexts prev-n gnome1 gnome2]
  (let [n (:number gnome1)
        present-count (+ (:present-count gnome1) (:present-count gnome2))]
    (update-presents nexts prev-n n present-count)))

(defn game-iteration
  "take-presents-and-remove-gnome"
  [nexts g-count prev-n]
  (let [gnome1 (get nexts prev-n)
        gnome2 (get nexts (:number gnome1))
        new-nexts (take-presents nexts prev-n gnome1 gnome2) ]
   {:nexts new-nexts :prev-n (:number gnome1)}))

(defn play-game
  ([nexts] (play-game {:nexts nexts :prev-n (count nexts)} (count nexts)))
  ([state g-count]
   (let [nexts (:nexts state)
         prev-n (:prev-n state)]
   (if (= 0 (mod g-count 50000)) (println g-count))
    (if (= 1 g-count) 
      (:number (first (vals nexts)))
       (recur (game-iteration nexts g-count prev-n) (dec g-count))))))

(defn part-one
  []
  (time (play-game (make-gnomes-m 3001330))))







(defn remove-target
  [nexts prev-n stealer before-target target new-count]
  (let [n (:number stealer)
        after-target (get nexts (:number target))
        new-stealer (assoc stealer :present-count new-count)
        nexts2 (assoc nexts prev-n new-stealer)
        nexts3 (assoc nexts2 (:number before-target) after-target)]
    (if (= (:number after-target) n)
        (dissoc nexts3 n)
        (dissoc nexts3 (:number target)))))

(defn take-presents2
  [nexts prev-n stealer before-target]
  (let [target (get nexts (:number before-target))
        present-count (+ (:present-count stealer) (:present-count target))]
    (remove-target nexts prev-n stealer before-target target present-count)))

(defn find-target
  [nexts curr remaining]
  (if (= 0 remaining) curr
    (recur nexts (get nexts (:number curr)) (dec remaining))))

(defn find-before-target
  "Find the gnome on the opposite in the circle"
  [nexts src g-count] 
   (let [dist (- (Math/round (Math/floor (/ g-count 2))) 1)]
         (find-target nexts src dist)))

(defn get-before-target
  [nexts g-count old-before-target]
  (if (even? g-count) old-before-target
    (let [target (get nexts (:number old-before-target))
          after-target (get nexts (:number target))]
      after-target)))

(defn game-iteration2
  "take-presents-and-remove-gnome"
  [nexts g-count prev-n before-target]
  (let [stealer (get nexts prev-n)
        new-nexts (take-presents2 nexts prev-n stealer before-target)
        new-before-target (get-before-target nexts g-count before-target) ]
   {:nexts new-nexts :prev-n (:number stealer) :before-target new-before-target}))

(defn play-game2
  ([nexts] (let [g-count (count nexts)
                 prev-n g-count
                 src (get nexts prev-n)]
             (play-game2 {:nexts nexts :prev-n prev-n :before-target (find-before-target nexts src g-count)} g-count)))
  ([state g-count]
   (let [nexts (:nexts state)
         prev-n (:prev-n state)
         before-target (:before-target state)]
   (if (= 0 (mod g-count 50000)) (println g-count))
    (if (= 1 g-count) 
      (:number (first (vals nexts)))
       (recur (game-iteration2 nexts g-count prev-n before-target) (dec g-count))))))

(defn part-two
  []
  (play-game2 (make-gnomes-m 3001330)))

(defn test-part-two
  []
  (play-game2 (make-gnomes-m 5)))


