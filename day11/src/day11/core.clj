(ns day11.core
  (:gen-class)
  (:require clojure.pprint
            [clojure.data.priority-map :refer :all]))
  (require '[clojure.string :as str])
  (require '[clojure.set :as set])
  (require '[clojure.data.priority-map :as pm])
(require '[clojure.math.combinatorics :as combo])
(use '[clojure.pprint :only (pprint)])


;The first floor contains a promethium generator and a promethium-compatible microchip.
;The second floor contains a cobalt generator, a curium generator, a ruthenium generator, and a plutonium generator.
;The third floor contains a cobalt-compatible microchip, a curium-compatible microchip, a ruthenium-compatible microchip, and a plutonium-compatible microchip.
;The fourth floor contains nothing relevant.


;
; COM CUM RUM PLM
; COG CUG RUG PLG
; PRG PRM

; promethium G0 M0
; cobalt     G1 M2
; curium     G1 M2
; ruthenium  G1 M2
; plutonium  G1 M2


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

(defn generator-counterpart
  [device]
  (assoc device :type "generator"))

(defn has-no-generator?
  [state device]
  (let [generator (generator-counterpart device)]
    (some generator state)))

(defn chips-ok?
  [state floor]
  (let [devices (filter #(= floor (:floor %)) state)
        chips (filter #(= "microchip" (:type %)) state)]))

(defn floor-ok?
  [device]
  (let [floor (:floor device)]
    (and (>= floor 0) (<= floor max-floor))))

(defn generator-counterpart
  [device]
  (assoc device :type "generator"))

(defn has-no-generator?
  [generators microchip]
  (let [generator (generator-counterpart microchip)]
    (not (contains? generators generator))))

(defn get-chips
  [devices]
  (set (filter #(= "microchip" (:type %)) devices)))

(defn get-generators
   [devices]
   (set (filter #(= "generator" (:type %)) devices)))

(defn without-generator
  [generators chips]
  (filter #(has-no-generator? generators %) chips))

(defn chips-ok?
  [devices floor]
  (let [on-floor (set (filter #(= floor (:floor %)) devices))
        chips (get-chips on-floor)
        generators (get-generators on-floor)
        without-generator (without-generator generators chips)]
        (not (and (> (count without-generator) 0) 
                  (> (count generators) 0)))))

(defn devices-ok?
  [devices]
  (every? true? (map #(chips-ok? devices %) all-floors)))

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



; en för varje unik med två 0, och en för varje unik kombintion med en i varje upp,
; och (evenutellt går att optimera bort) en för varje ed en 0 med endast en uppdaterad

; map inc2 pairs + 


; behöver typ replaceone
;incenivarje cc = 1010 1001 0110 0101
; om båda är = 0, returnera ett state där dessa två par har dessa positioner ökade
; mxa fyra nya statejjhh

; cc = (combo/combinations pairs 2)
;(map (incenivarje pairs) cc)

; + samma som ovan fast för bara ett par i taget, där man ökar en 

; +
;  map 

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
        {:pairs  new-pairs :elevator (+ sign floor)})
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
              (flatten 
                (concat up-states-2 up-states-1 down-states-1 down-states-2 ))))))

(defn neighbors
  [state S]
      (do (let [possible (possible-moves state)
         ;   p (println "in neighbors")
            ]
       (filter #(not (contains? (set S) %)) possible))))

(defn follow 
  ([n parents1] (follow n parents1 []))
  ([n parents1 path]
    (let [parent (get parents1 n)]
          (if parent 
              (recur parent parents1 (cons parent path))
              path))))

(defn assoc-multiple
  [map1 keys1 value]
  (reduce #(assoc %1 %2 value) map1 keys1))

(defn average
  [state]
  (let [nn (cons (:elevator state) (flatten (:pairs state)))]
    (/ (reduce + nn) (+ 1 (count nn)))))

  

(defn state-cost-factor
  [x]
;  1)
 ; (* 100 (- max-floor (:elevator x))))

  (let [fact (/ (average x) max-floor)]
    (/ 1000000 (* fact fact))))

(defn cost
  [q x]
  (* (state-cost-factor x) (get q x)))

(defn get-min
  [q]
  (first (sort-by #(cost q %) (keys q))))

(defn get-dist
  [dists k]
   (let [dist (get dists k)]
     (if dist dist Integer/MAX_VALUE)))

(defn should-terminate? 
  [q dists goal]
;  (or 
    (= (count q) 0)
   ;   (> (get dists (get-min q)) (get dists goal))))
  )


(defn searched
  [t]
  (count (:dists t)))

(defn shortest-path
  [src goal]
    (loop [q (assoc {} src 0)
           dist {src 0 goal Integer/MAX_VALUE}
           prev {}]
      (do (let [the-min  (get-min q)]
          (pprint (str the-min " distance: " (get dist the-min)  " count q: " (count (keys q))
                       " cost: " (float (cost q the-min))  " searched: "      (count (keys dist))))))
      (if (should-terminate? q dist goal) 
        {:dists dist :prevs prev}
        (let [u  (get-min q)
              ll (loop [nn (neighbors u {})
                        q2 (dissoc q u)
                        dist2 dist
                        prev2 prev]
                      (if (= 0 (count nn))
                          [q2 dist2 prev2]
                          (let [v (first nn)
                                alt (+ 1 (get dist2 u))
                                dist-v (get-dist dist2 v)]
                            (if (< alt dist-v) 
                                    (do 
                                     ; (if (not (contains? (set (vals dist)) alt)) (println alt))
                                        (if (= goal v) 
                                          (do 
                                            (pprint (str "found goal at: " alt))
                                            (pprint v)
                                            ))
                                      (recur (rest nn)
                                       (assoc q2 v alt)
                                       (assoc dist2 v alt)
                                       (assoc prev2 v u)))
                                    (recur (rest nn)
                                       q2
                                       dist2
                                       prev2)))))]
                        (recur (dissoc (get ll 0) u) ; remove u from q
                               (get ll 1)
                               (get ll 2))))))

(defn follow-path
  ([prevs curr path]
  (let [prev (get prevs curr)]
    (if prev
      (recur prevs prev (cons curr path))
      path
      ))))


