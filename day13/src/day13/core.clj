(ns day13.core
  (:gen-class)
  (:require clojure.pprint
            [clojure.data.priority-map :refer :all]))
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.data.priority-map :as pm])
(require '[clojure.math.combinatorics :as combo])
(use '[clojure.pprint :only (pprint)])

; pairs: [generator, microchip]
(def start {:x 1  :y 1})
(def goal  {:x 31 :y 39})

(def favorite-num 1350)
;(def favorite-num 10)

(defn to-state 
  [x y]
  {:x x :y y})

(defn combinations
  [dd n]
  (if (< (count dd) n)
    '()
    (combo/combinations dd n)))

(defn calculate
  [state]
  (let [x (:x state)
        y (:y state)]
    (+ (* x x) (* 3 x) (* 2 x y) y (* y y) favorite-num)))

(defn as-bits
  [n]
  (Integer/toBinaryString n))

(defn nbr-ones
  [b]
  (count (str/replace b #"0" "")))

(defn positive?
  [state]
  (and (>= (:x state) 0)
       (>= (:y state) 0)))

(defn valid-state?
  [state]
    (and (positive? state)
         (even? (nbr-ones (as-bits (calculate state))))))

(defn calc-range
  [y cols]
  (map (fn[n] {:x n :y y}) (range cols)))

(defn representation
  [state]
  (if (valid-state? state) "." "#"))

(defn representation-line
  [line]
  (str/join (map representation line)))

(defn get-map
  [n]
  (map #(calc-range % n) (range n)))

(defn print-map
  [n]  
  (map println (map #(representation-line %) (get-map n))))



(defn adjacent
  [state]
  #{(update state :x inc)
    (update state :x dec)
    (update state :y inc)
    (update state :y dec) })

(defn possible-moves
  [state]
  (filter valid-state? (adjacent state)))

(defn neighbors
  [state S]
  (filter #(not (contains? (set S) %)) (possible-moves state)))

(defn should-terminate?
  [open dists goal]
  (let [closest (peek dists)
        cdist (second closest)
        gdist (get dists goal)]
    (or
     (= (count open) 0)
     (= (first (peek open)) goal)
     (= cdist gdist))))

(defn get-dist
  [dists k]
   (let [dist (get dists k)]
     (if dist dist Integer/MAX_VALUE)))

(defn backtrack
  ([prevs curr] (backtrack prevs curr []))
  ([prevs curr path]
   (let [prev (get prevs curr)]
     (if prev
       (recur prevs prev (cons curr path))
       path))))

(defn shortest-path
  [heuristic src goal]
   (loop [open (priority-map src (heuristic src goal))
          closed #{}
          fscores (priority-map src (heuristic src goal))
          dists (priority-map src 0 goal Integer/MAX_VALUE)
          prevs {}]
     (if (should-terminate? open dists goal)
       (backtrack prevs goal)
       (let [curr (first (peek open))
             res (loop [open2 (pop open)
                       closed2 (conj closed curr)
                       nn (neighbors curr closed2)
                       fscores2 fscores
                       dists2 dists
                       prevs2 prevs]
                  (if (= 0 (count nn))
                    [open2 closed2 fscores2 dists2 prevs2]
                    (let [v (first nn)
                          tentative (+ 1 (get dists2 curr))
                          dist-v (get-dist dists2 v)]
                      (if (< tentative dist-v)
                          (recur (assoc open2 v (+ tentative (heuristic v goal)))
                                 closed2
                                 (rest nn)
                                 (assoc fscores2 v (+ tentative (heuristic v goal)))
                                 (assoc dists2 v tentative)
                                 (assoc prevs2 v curr))
                        (recur open2
                               closed2
                               (rest nn)
                               fscores2
                               dists2
                               prevs2)))))]
         (recur (get res 0)
                (get res 1)
                (dissoc (get res 2) curr) ; remove curr from fscores
                (get res 3)
                (get res 4))))))

(defn searched
  [t]
  (count (:dists t)))

(defn heuristic1
  [src goal]
  (+ (Math/abs (- (:y goal) (:y src)))
     (Math/abs (- (:x goal) (:x src)))))
  

(defn part-one
  []
  (shortest-path heuristic1 start goal))

(defn should-terminate2?
  [open]
     (= (count open) 0))





(defn representation2
  [state known]
  (if (contains? known state) "O" 
      (if (valid-state? state) "." "#")))

(defn representation-line2
  [line known]
  (str/join (map #(representation2 % known) line)))

(defn draw-graph
  [max1 known]
  (map println (map #(representation-line2 % known) (get-map max1))))

(defn is-weird?
  [state]
  (and (= (:y state) 2) (= 23 (:x state))))


(defn build-graph
  [src max-depth]
   (loop [open #{src}
          closed #{}
          dists (priority-map src 0 goal Integer/MAX_VALUE) ]
     (if (should-terminate2? open)
       closed
       (let [curr (first open)]
            (if (> (get dists curr) max-depth)
              (recur (drop 1 open)
                     closed
                     dists)
              (let [res (loop [open2 (drop 1 open)
                       closed2 (conj closed curr)
                       nn (neighbors curr #{})
                       dists2 dists ]
                  (if (= 0 (count nn))
                    [open2 closed2 dists2]
                    (let [v (first nn)
                          tentative (+ 1 (get dists2 curr))
                          dist-v (get-dist dists2 v)]
                      (if (< tentative dist-v)
                          (recur (conj open2 v)
                                 closed2
                                 (rest nn)
                                 (assoc dists2 v tentative))
                        (recur open2
                               closed2
                               (rest nn)
                               dists2)))))]
         (recur (get res 0)
                (get res 1)
                (get res 2))))))))

(defn part-two 
  []
  (count (build-graph start 50)))

(defn part-two-debug
  []
  (draw-graph 50 (build-graph start 50)))





