(ns day11.core
  (:gen-class)
  (:require clojure.pprint))
  (require '[clojure.string :as str])
  (require '[clojure.set :as set])
(require '[clojure.math.combinatorics :as combo])
(use '[clojure.pprint :only (pprint)])

(defn floor-ok?
  [device]
  (let [floor (:floor device)]
    (and (>= floor 0) (<= floor 4))))

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

(defn valid-state?
  [state]
  (if (or 
        (some #(not (floor-ok? %)) (:devices state))
        (or (< (:elevator state) 0 )
            (> (:elevator state) max-floor)))
    false
    (devices-ok? (:devices state))))

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

; return two new states where the devices have been moved up and down
(defn up-and-down
  [devices to-move elevator]
  (let [removed (set (remove (set to-move) devices))
        moved-up (set (map #(change-floor % inc) to-move))
        moved-down (set (map #(change-floor % dec) to-move))
        down-devices (apply conj removed moved-down)
        up-devices (apply conj removed moved-up)
        down-state {:devices down-devices :elevator (dec elevator)}
        up-state {:devices up-devices :elevator (inc elevator)}]
     (filter #(valid-state? %) [down-state up-state])))

(defn possible-moves-floor 
  [state]
  (let [elevator (:elevator state)
        devices (:devices state)
        at-floor (filter #(= elevator (:floor %)) devices)
        comb-1 (combinations at-floor 1)
        comb-2 (combinations at-floor 2)
        combs (apply conj comb-1 comb-2)]
    (flatten-one (map #(up-and-down devices % elevator) combs))))

(defn possible-moves
  [state]
  (let [ moves (possible-moves-floor state)]
    moves))

(defn goal-state?
  [state]
  (let [devices (:devices state)
        res (every? #(= goal-floor (:floor %)) devices)
        ;ppp (if res (println nbr-moves))
        ]
    res))

(def visited (ref #{}))

(defn get-visited []
  @visited)

(defn update-visited [v]
  (dosync (ref-set visited v)))

(def results (ref #{}))

(defn get-results [] @results)

(defn update-results [v]
  (dosync (ref-set results v)))

(defn make-result
  [steps path]
  {:steps steps :path path})

(defn get-best
  [results]
  (let [sorted (sort-by :steps (filter #(not (= not-found (:steps %))) results))]
    (if (= 0 (count sorted)) 
      {:steps not-found}
      (first sorted))))

(defn search-recursive
  [state nbr-moves path]
;  (pprint (str "nbr visited: " (count (get-visited))))
;  (if (contains? tt state) (pprint state))
 ; (print "Current state: ")
; (pprint state)
 ; (if (= t9 state) (println "at t9"))
  (if (goal-state? state) (make-result nbr-moves (conj path state)) 
     (let [new-visited (conj (get-visited) state)
           x (update-visited new-visited)
           new-path (conj path state)
           all-possible (possible-moves state)
           moves (filter #(not (contains? new-visited %)) all-possible)
            
          ;ff  (if (= t9 state) (println "all-possible has t10?"))
          ;ff  (if (= t9 state) (println (contains? (set all-possible) t10)))
          ; xx (if (contains? tt state) (print "Possible moves: " ))
          ; xxx (if (contains? tt state) (pprint all-possible))
        ;   ll (print \newline)
         ;  l1 (pprint "Possible moves: " )
          ; l2 (pprint all-possible)
           ;x9 (println (read-line))

           results (map #(search-recursive % (inc nbr-moves) new-path) moves)
           ]
       (if (= 0 (count moves))
         {:steps not-found}
          (get-best results))
     )))

(defn neighbors
  [state S]
      (let [possible (possible-moves state)]
       (filter #(not (contains? S %)) possible)))

(def Q (ref #{}))
(def S (ref #{}))
(def Parents (ref {}))

(defn get-Q [] @Q)
(defn get-S [] @S)
(defn get-Parents [] @Parents)

(defn update-Q [q]
  (dosync (ref-set Q (set q))))

(defn update-S [s]
  (dosync (ref-set S (set s))))

(defn update-Parents [p]
  (dosync (ref-set Parents p)))

(defn first-Q [] 
  (let [v (first (get-Q))]
    (update-Q (rest (get-Q)))
    v))
(defn first-S [] 
  (let [v (first (get-S))]
    (update-S (rest (get-S)))
    v))

(defn search-iterative
  [state]
  (update-S [state])
  (update-Q [state])
  (update-Parents {})
    (loop [q (get-Q)]
      (let [current (first-Q)
            neighbors (neighbors current (get-S))]
          (doseq [n neighbors]
            (if (= t1 current) (pprint (count (get-Parents))))
            (if (= t1 current) (pprint (get-Parents)))
            (if (= t1 current) (pprint (current (get-Parents))))
            (update-S (conj (get-S) n))
            (update-Parents (assoc (get-Parents) n current))
            (update-Q (conj (get-Q) n)))
      (if (goal-state? current) 
        {:goal current :parents (get-Parents)}
        (recur (get-Q))))))

(defn follow 
  ([n parents1] (follow n parents1 []))
  ([n parents1 path]
    (let [parent (get parents1 n)]
          (if parent 
              (recur parent parents1 (cons parent path))
              path))))

(defn do-search
  [state]
  (update-visited #{})
  (let [results (search-recursive state 0 [])]
    results))



  

(defn best-solution
  [solutions]
  (apply min (filter #(> % 0) solutions)))

(def small-state
  { :devices
    #{{:type "generator" :substance "promethium" :floor 0} 
     {:type "generator" :substance "einsteinium" :floor 2}
     {:type "generator" :substance "hydrogen" :floor 2}}
   :elevator 0})

(def s1{:devices
  #{{:type "generator", :substance "hydrogen", :floor 3}
    {:type "generator", :substance "promethium", :floor 2}},
  :elevator 2})



(def test-state
  { :devices
    #{{:type "microchip" :substance "hydrogen" :floor 0} 
     {:type "microchip" :substance "lithium" :floor 0}
     {:type "generator" :substance "hydrogen" :floor 1} 
     {:type "generator" :substance "lithium" :floor 2}}
    :elevator 0})

(def t1
  { :devices
    #{{:type "microchip" :substance "hydrogen" :floor 1} 
     {:type "microchip" :substance "lithium" :floor 0}
     {:type "generator" :substance "hydrogen" :floor 1} 
     {:type "generator" :substance "lithium" :floor 2}}
    :elevator 1})

(def t2
  { :devices
    #{{:type "microchip" :substance "hydrogen" :floor 2} 
     {:type "microchip" :substance "lithium" :floor 0}
     {:type "generator" :substance "hydrogen" :floor 2} 
     {:type "generator" :substance "lithium" :floor 2}}
    :elevator 2})

(def t3
  { :devices
    #{{:type "microchip" :substance "hydrogen" :floor 1} 
     {:type "microchip" :substance "lithium" :floor 0}
     {:type "generator" :substance "hydrogen" :floor 2} 
     {:type "generator" :substance "lithium" :floor 2}}
    :elevator 1})

(def t4
  { :devices
    #{{:type "microchip" :substance "hydrogen" :floor 0} 
     {:type "microchip" :substance "lithium" :floor 0}
     {:type "generator" :substance "hydrogen" :floor 2} 
     {:type "generator" :substance "lithium" :floor 2}}
    :elevator 0})

(def t5
  { :devices
    #{{:type "microchip" :substance "hydrogen" :floor 1} 
     {:type "microchip" :substance "lithium" :floor 1}
     {:type "generator" :substance "hydrogen" :floor 2} 
     {:type "generator" :substance "lithium" :floor 2}}
    :elevator 1})

(def t6
  { :devices
    #{{:type "microchip" :substance "hydrogen" :floor 2} 
     {:type "microchip" :substance "lithium" :floor 2}
     {:type "generator" :substance "hydrogen" :floor 2} 
     {:type "generator" :substance "lithium" :floor 2}}
    :elevator 2})

(def t7
  { :devices
    #{{:type "microchip" :substance "hydrogen" :floor 3} 
     {:type "microchip" :substance "lithium" :floor 3}
     {:type "generator" :substance "hydrogen" :floor 2} 
     {:type "generator" :substance "lithium" :floor 2}}
    :elevator 3})

(def t8
  { :devices
    #{{:type "microchip" :substance "hydrogen" :floor 2} 
     {:type "microchip" :substance "lithium" :floor 3}
     {:type "generator" :substance "hydrogen" :floor 2} 
     {:type "generator" :substance "lithium" :floor 2}}
    :elevator 2})

(def t9
  { :devices
    #{{:type "microchip" :substance "hydrogen" :floor 2} 
     {:type "microchip" :substance "lithium" :floor 3}
     {:type "generator" :substance "hydrogen" :floor 3} 
     {:type "generator" :substance "lithium" :floor 3}}
    :elevator 3})

(def t10
  { :devices
    #{{:type "microchip" :substance "hydrogen" :floor 2} 
     {:type "microchip" :substance "lithium" :floor 2}
     {:type "generator" :substance "hydrogen" :floor 3} 
     {:type "generator" :substance "lithium" :floor 3}}
    :elevator 2})

(def t11
  { :devices
    #{{:type "microchip" :substance "hydrogen" :floor 3} 
     {:type "microchip" :substance "lithium" :floor 3}
     {:type "generator" :substance "hydrogen" :floor 3} 
     {:type "generator" :substance "lithium" :floor 3}}
    :elevator 3})

(def t0 test-state)

(def tt #{t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11})

(def initial-state
  {:devices
    #{{:type "generator" :substance "promethium" :floor 0} 
     {:type "microchip" :substance "promethium" :floor 0}
     {:type "generator" :substance "cobalt" :floor 1} 
     {:type "generator" :substance "curium" :floor 1}
     {:type "generator" :substance "ruthenium" :floor 1}
     {:type "generator" :substance "plutonium" :floor 1}
     {:type "microchip" :substance "cobalt" :floor 2}
     {:type "microchip" :substance "curium" :floor 2}
     {:type "microchip" :substance "ruthenium" :floor 2}
     {:type "microchip" :substance "plutonium" :floor 2}}
   :elevator 0})





