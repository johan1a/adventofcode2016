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
  (let [gnomes (map make-gnome (range n))]
        (make-nexts gnomes 1 n)))

(defn delete
  "Delete n2"
  [nexts n1 n2]
  (dissoc (assoc nexts n1 (get nexts n2)) n2))

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
        new-nexts (take-presents nexts prev-n gnome1 gnome2)]
   {:nexts new-nexts :prev-n (:number gnome1)}))

(defn play-game
  ([nexts] (play-game {:nexts nexts :prev-n (count nexts)} (count nexts)))
  ([state g-count]
   (let [nexts (:nexts state)
         prev-n (:prev-n state)]
;   (if (= 0 (mod g-count 1000)) (time (pprint (count nexts))))
  (if (= 1 g-count) 
    (first (vals nexts))
     (recur (game-iteration nexts g-count prev-n) (dec g-count))))))

(defn part-one
  []
  (play-game (make-gnomes 3001330)))

