(ns day11.core
  (:gen-class)
  (:require clojure.pprint
            [clojure.data.priority-map :refer :all]))
  (require '[clojure.string :as str])
  (require '[clojure.set :as set])
  (require '[clojure.data.priority-map :as pm])
(require '[clojure.math.combinatorics :as combo])
(use '[clojure.pprint :only (pprint)])

; pairs: [generator, microchip]
(def x0
  {:pairs
    [ [ 0  0 ]
      [ 1  2 ]
      [ 1  2 ]
      [ 1  2 ]
      [ 1  2 ] ]
   :elevator 0})

(def goal0
  {:pairs
    [ [ 3  3 ]
      [ 3  3 ]
      [ 3  3 ]
      [ 3  3 ]
      [ 3  3 ] ]
   :elevator 3})

(def x1
  {:pairs
    [ [ 0  0 ]
      [ 1  2 ]
      [ 1  2 ]
      [ 1  2 ]
      [ 1  2 ] 
      [ 0  0 ] 
      [ 0  0 ] ]
   :elevator 0})

(def goal1
  {:pairs
    [ [ 3  3 ]
      [ 3  3 ]
      [ 3  3 ]
      [ 3  3 ]
      [ 3  3 ] 
      [ 3  3 ] 
      [ 3  3 ] ]
   :elevator 3})


(def xt
  {:pairs
    [ [ 1 0 ]
      [ 2 0 ] ]
   :elevator 0})

(def goalt
  {:pairs
    [ [ 3 3 ]
      [ 3 3 ] ]
   :elevator 3})

(def all-floors [0 1 2 3])
(def not-found -1)
(def goal-floor 3)
(def max-floor 3)

(defn flatten-one
  "Flatten one level"
  [ll]
  (apply concat ll))

(defn combinations
  [dd n]
  (if (< (count dd) n) 
    '()
  (combo/combinations dd n)))

(defn change-floor
  [device d]
  (update device :floor d ))

; variations to increase / decrease one pair
(def offsets-1    [[0 1]
                   [1 0]
                   [1 1]])

;variations to increase / decrease a combinations of two pars.
(def offsets-2      [
;                   [0 0 0 0]
                   [0 0 0 1] 
                   [0 0 1 0] 
                   [0 0 1 1]
                   [0 1 0 0]
                   [0 1 0 1]
                   [0 1 1 0]
;                   [0 1 1 1]
                   [1 0 0 0]
                   [1 0 0 1]
                   [1 0 1 0]
                   [1 1 0 0]]
;                   [1 1 0 1]
;                   [1 1 1 0]
;                   [1 1 1 1]])
)

(defn mul
  [v1 v2]
  [(* (first v1) (first v2)) (* (second v1) (second v2))])

(defn matches-offset
  [pair offset floor]
  (= (mul offset [floor floor]) (mul pair offset)))

(defn add-offset-1
  [pair offset sign]
   (map + pair (mul offset [sign sign])))

(defn make-offset-state-1
  [pairs pair offset floor sign]
  (if (matches-offset pair offset floor)
      (let [
            new-pair (add-offset-1 pair offset sign)
            index (.indexOf pairs pair)
            new-pairs (assoc pairs index new-pair)
            ]
        {:pairs new-pairs :elevator (+ sign floor)})
      false))

(defn make-offset-state-2
  [pairs pair-comb offset floor sign]
  (let [offset1 (take 2 offset)
        offset2 (drop 2 offset)
        state (make-offset-state-1 pairs (first pair-comb) offset1 floor sign)]
        (if state
            (let [pairs2 (:pairs (make-offset-state-1 pairs (first pair-comb) offset1 floor sign))
                  res (make-offset-state-1 pairs2 (second pair-comb) offset2 floor sign)]
            res)
            false)))

; move parts in one pair
(defn move-one-pair
  [pairs pair floor sign]
  (filter #(not (false? %)) (map #(make-offset-state-1 pairs pair % floor sign) offsets-1)))

; move parts in two different pairs
(defn move-two-pairs
  [pairs pair-comb floor sign]
  (filter #(not (false? %)) (map #(make-offset-state-2 pairs pair-comb % floor sign) offsets-2)))


(defn pair-ok?
  [pairs pair]
  (if (= (first pair) (second pair))
      true
      (not-any? #(= (second pair) (first %)) (disj (set pairs) pair))))

(defn pairs-ok?
  [pairs]
  (every? #(pair-ok? pairs %) pairs))

(defn floors-ok?
  [pair]
  (and (not-any? #(< % 0) pair)
       (not-any? #(> % max-floor) pair)))

(defn elevator-ok?
  [state]
  (or (>= (:elevator state) 0 )
      (<= (:elevator state) max-floor)))

(defn valid-state?
  [state]
  (if (or 
        (some #(not (floors-ok? %)) (:pairs state))
        (not (elevator-ok? state)))
    false
    (pairs-ok? (:pairs state))))

(defn sort-state
  [state]
  {:pairs (vec (sort (map vec (:pairs state)))) :elevator (:elevator state)})

(defn possible-moves
  [state]
  (let [pairs (:pairs state)
        floor (:elevator state)
        comb-1 (distinct pairs)
        comb-2 (combinations pairs 2)
        up-states-1 (map #(move-one-pair pairs % floor 1) comb-1)
        down-states-1 (map #(move-one-pair pairs % floor -1) comb-1)
        up-states-2 (map #(move-two-pairs pairs % floor 1) comb-2)
        down-states-2 (map #(move-two-pairs pairs % floor -1) comb-2)
        ]
    (filter valid-state?
            (distinct 
              (map sort-state
                (flatten 
                  (concat up-states-2 up-states-1 down-states-1 down-states-2 )))))))

(defn neighbors
  [state S]
      (do (let [possible (possible-moves state)]
       (filter #(not (contains? (set S) %)) possible))))

(defn assoc-multiple
  [map1 keys1 value]
  (reduce #(assoc %1 %2 value) map1 keys1))

(defn get-dist
  [dists k]
   (let [dist (get dists k)]
     (if dist dist Integer/MAX_VALUE)))

(defn searched
  [t]
  (count (:dists t)))

(defn not-contains?
  [s v]
  (not (contains? s v)))

(defn should-terminate? 
  [open dists goal]
  (let [closest (peek dists)
        cdist (second closest)
        gdist (get dists goal)]
    (or
      (= (first (peek open)) goal) 
      (= (count open) 0)
      (= cdist gdist))))

(defn get-time
  []
  (System/currentTimeMillis))

(defn diff
  [a b]
  (reduce + (map - (flatten (:pairs a)) (flatten (:pairs b)))))

(defn prune-singles
  [curr nn]
  (let [downs (filter #(< (:elevator %) (:elevator curr)) nn)
        ups (filter #(> (:elevator %) (:elevator curr)) nn)
        twos (filter #(= 2 (diff % curr)) ups)
        ones (filter #(= 1 (diff % curr)) ups)]
        (if (> (count twos) 0)
            (concat twos downs)
            (concat ones downs))))

(defn shortest-path
  ([heuristic src goal] (shortest-path heuristic src goal (get-time)))
  ([heuristic src goal start-time]
    (loop [open (priority-map src (heuristic src goal))
           closed #{}
           fscores (priority-map src (heuristic src goal))
           dist (priority-map src 0 goal Integer/MAX_VALUE)
           prev {}]
;      (pprint (get prev (first (peek open)) ))
;      (pprint (get dist (get prev (first (peek open))) ))
      (if (should-terminate? open dist goal) 
        {:dists dist :prevs prev}
        (let [curr (first (peek open))
              ll (loop [open2 (pop open)
                        closed2 (conj closed curr)
                        nn (prune-singles curr (neighbors curr closed2))
                        fscores2 fscores
                        dist2 dist
                        prev2 prev]
                      (if (= 0 (count nn))
                          [open2 closed2 fscores2 dist2 prev2 ]
                          (let [v (first nn)
                                tentative (+ 1 (get dist2 curr))
                                dist-v (get-dist dist2 v)]
                            (if (< tentative dist-v) 
                                    (do 
;                                      (if (not (contains? (set (vals dist)) tentative)) (println tentative))
                                        (if (= goal v) 
                                          (do 
                                            (pprint (str "found goal at: " tentative))
                                            (pprint v)
                                            (pprint (str "Elapsed time: " (- (get-time) start-time) " msecs"))
                                            (pprint (str "in open: " (count open2)))
;                                            (read-line)
                                            ))
                                      (recur (assoc open2 v (+ tentative (heuristic v goal)))
                                       closed2
                                       (rest nn)
                                       (assoc fscores2 v (+ tentative (heuristic v goal)))
                                       (assoc dist2 v tentative)
                                       (assoc prev2 v curr)))
                                    (recur open2
                                       closed2
                                       (rest nn)
                                       fscores2
                                       dist2
                                       prev2)))))]
                        (recur (get ll 0) 
                               (get ll 1)
                               (dissoc (get ll 2) curr) ; remove curr from fscores
                               (get ll 3)
                               (get ll 4)))))))

(defn heuristic1
  [src goal]
  (* 2 (reduce + (map - (flatten (:pairs goal)) (flatten (:pairs src))))))

(defn heuristic2
  [src goal]
  (let [diffs (map - (flatten (:pairs goal)) (flatten (:pairs src)))
        sum1  (reduce + diffs)
        ]
   (reduce + (map #(* 1.7 %) diffs))))

(defn heuristic3
  [src goal]
  (* 1.8 (reduce + (map - (flatten (:pairs goal)) (flatten (:pairs src))))))

(defn part-one
  []
  (shortest-path heuristic1 x0 goal0))

(defn part-two
  []
  (time (shortest-path heuristic3 x1 goal1)))

(defn follow-path
  ([prevs curr path]
  (let [prev (get prevs curr)]
    (if prev
      (recur prevs prev (cons curr path))
      path
      ))))


