(ns day22.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer :all]))

(defn parse-num
  [t-num]
  (Integer/parseInt (subs t-num 0 (- (count t-num) 1))))

(defn parse-line
  [line]
  (let [splitted (str/split line #"\s+")
        path (nth splitted 0)
        x (Integer/parseInt (first (str/split (second (str/split path #"/dev/grid/node-x")) #"-y")))
        y (Integer/parseInt (last (str/split path #"-y")))
        size (parse-num (nth splitted 1))
        used (parse-num (nth splitted 2))
        avail (parse-num (nth splitted 3))
        use- (parse-num (nth splitted 4))]
    {:pos [x y] :size size :used used :avail avail :use% use-}))

(defn get-input
  [file-name]
  (map parse-line (str/split (slurp file-name) #"\n")))

(defn sort-by-
  [nodes attr]
  (sort-by (fn [node] (attr node)) nodes))

(defn is-viable1?
  [a b]
  (and (not (= a b))
       (> (:used a) 0)
       (<= (:used a) (:avail b))))

(defn viable-targets1
  [nodes node]
  (filter #(is-viable1? node %) nodes))

(defn count-viable
  [nodes]
   (reduce + (map #(count (viable-targets1 nodes %)) nodes)))

(defn part-one
  []
  (count-viable (get-input "input.txt")))











; Part two

(defn representation
  [node-map data-pos k]
  (let [node (get node-map k)
        empty- (= 0 (:used node))
        full- (>= (:used node) 490 )
        goal-data (= data-pos (:pos node))]
    (cond empty- "_" 
          full- "#"
          goal-data "G"
          :else ".")))

(defn max-col
  [keys-]
  (apply max (map first keys-)))

(defn visualize
  [state]
  (let [node-map (:node-map state)
        data-pos (:data-pos state)
        keys- (sort-by #(vec (reverse %)) (map first node-map))
        visuals (map #(representation node-map data-pos %) keys-)
        nbr-cols (+ 1 (max-col keys-))]
    (map println (map str/join (partition nbr-cols visuals)))))

(defn initial-data-pos 
  [node-map]
  (let [lower-right (last (sort (keys node-map)))
        x (first lower-right)
        y 0]
    [x y]))


(defn make-node-map
  ([nodes] (make-node-map nodes {}))
  ([nodes m]
   (if (= 0 (count nodes)) m
     (let [n (first nodes)]
       (recur (rest nodes) (assoc m (:pos n) n))))))

(defn abs
  [x]
  (if (< x 0) (abs (- x)) x)) 

(def viable-diffs 
  #{[0 1] [1 0]})

(defn adjacent?
  [a b]
  (let [diff (map (comp abs -) (:pos a) (:pos b))]
    (contains? viable-diffs diff)))

(defn is-viable?
  [a b]
  (and (not (= a b))
       (adjacent? a b)
       (> (:used a) 0)
       (<= (:used a) (:avail b))))

(defn viable-targets
  [all-nodes node]
  (doall (if (not (contains? (set all-nodes) node)) []
    (filter #(is-viable? node %) all-nodes))))

(defn possible-moves-node
  [all-nodes node]
  (map :pos (viable-targets all-nodes node)))

(defn possible-moves
  ([nodes] (possible-moves nodes nodes {} (get-now)))
  ([all-nodes to-check m start ]
   (if (empty? to-check) m
     (let [
           n (doall (first to-check))
           possible (doall (possible-moves-node all-nodes n))
           updated-m (doall (assoc m (:pos n) possible))
           new-m (doall (if (empty? possible) m updated-m))
;           x (pprint (- (get-now) start))

           ]
      (recur all-nodes (rest to-check) new-m start )))))

(def goal-pos [0 0])

(defn abs
  [x]
  (if (> x 0) x (* x -1)))

(defn add
  [pos dir]
  (map + pos dir))

(defn sub-state
  [a b]
  (map - (:pos a) (:pos b)))

(defn clear-data
  [node]
  (let [pos (:pos node)
        size (:size node)]
  {:pos pos
   :size size
   :used 0
   :avail size
   :use% 0}))

(defn percentage
  [ratio]
  (Math/round (* 100 (float ratio))))

(defn add-data
  [node data-size]
  (let [new-used (+ data-size (:used node))
        new-avail (- (:avail node) data-size)
        new-use%  (percentage (/ new-used (:size node)))]
        {:pos (:pos node)
         :size (:size node)
         :used new-used
         :avail new-avail
         :use% new-use%}))

(def pos-offsets [[0 1] [1 0] [0 -1] [-1 0]])

(defn add-pos 
  [a b]
  (map + a b)) 

(defn neighboring-nodes
  [pos]
  (map #(add-pos pos %) pos-offsets))

(defn dissoc-many
  [m values]
  (if (empty? values) m
    (recur (dissoc m (first values)) (rest values))))

(defn pos-neighbors
  [move]
  (let [src-neighbors (neighboring-nodes (:src move))
        dest-neighbors (neighboring-nodes (:dest move))
        all-neighbors (set (concat src-neighbors dest-neighbors))]
    all-neighbors))

(defn delete-around
  "Delete the old possible moves that could be changed because of the move"
  [state all-neighbors]
  (let [old-possible-moves (:possible-moves state)
        new-possible-moves (dissoc-many old-possible-moves all-neighbors)]
    new-possible-moves))

(defn update-possible
  [state pos-to-check]
  (let [ start (get-now)
        all-nodes (doall (vals (:node-map state)))
        nodes-to-check (doall (map #(get (:node-map state) %) pos-to-check))
        new-possible (doall (possible-moves all-nodes nodes-to-check (:possible-moves state) (get-now) ))
        ss (pprint "----------_")
        ;x (pprint (- (get-now) start))
        ]
    (assoc state :possible-moves new-possible)))

(defn update-possible-moves
  [state]
  (let [start (get-now)
        prev-move (:prev-move state)
        src (:src prev-move)
        dest (:dest prev-move)
        possible-moves-old (doall (:possible-moves state))
        all-neighbors (doall (pos-neighbors prev-move))
        cleared (doall (delete-around state all-neighbors))
        with-cleared (doall (assoc state :possible-moves cleared))
        updated (update-possible with-cleared all-neighbors)
        ;x (pprint (- (get-now) start))
        ]
    updated))

(defn do-move
  "Returns a new state where data from src has been moved to dest"
  [state src dest]

  (let [start (get-now) 
        node-map (:node-map state)
        n1 (get node-map src)
        n2 (get node-map dest)
        new-n1 (clear-data n1)
        data-size (:used n1)
        new-n2 (doall (add-data n2 data-size))
        new-node-map (doall (assoc (assoc node-map src new-n1) dest new-n2))
        old-data-pos (:data-pos state)
        new-data-pos (doall (if (= old-data-pos src) dest old-data-pos))
        state-to-update {:node-map new-node-map 
                           :data-pos new-data-pos
                           :prev-move {:src src :dest dest}
                           :possible-moves (:possible-moves state)}
        updated-state (doall (update-possible-moves state-to-update))
        ;x (pprint (- (get-now) start))
        ]
    updated-state))

(defn do-moves
  "Return a new state for each move"
  [state moves]
  (let [src (first moves)
        dests (second moves)]
     (map #(do-move state src %) dests)))

(defn clean-state
  "Return a cleaned state for comparison"
  [state]
  (dissoc (dissoc state :possible-moves) :prev-move))

(defn neighbors
  ([state] (neighbors state #{}))
  ([state closed]
  (if (= (:data-pos state) goal-pos) []
    (let [possible (:possible-moves state)
          kk (mapcat #(do-moves state %) possible)
;          x (time (pprint (count kk)))
          nn (flatten kk)
          unique (filter #(not (contains? closed (clean-state %))) nn)
          ]
   unique))))

(defn get-dist
  [dists k]
   (let [dist (get dists k)]
     (if dist dist Integer/MAX_VALUE)))

(defn should-terminate?
  [open dists]
      (= 0 (count open)))

(defn dist-to-goal
  [dists]
  (let [goal-state (first (filter #(= [0 0] (:data-pos %)) (keys dists)))]
  (get dists goal-state)))

(defn get-now [] (System/currentTimeMillis))

(defn check-neighbors
  [heuristic open closed nn fscores dists curr start]
    (if (empty? nn)
      [open closed fscores dists]
      (let [v (first nn)
            tentative (+ 1 (get dists curr))
            dist-v (get-dist dists v) ]
        (if (< tentative dist-v)
              (recur heuristic
                   (assoc open v (+ tentative (heuristic v)))
                   closed
                   (rest nn)
                   (assoc fscores v (+ tentative (heuristic v)))
                   (assoc dists v tentative)
                   curr
                   start)
              (recur heuristic 
                 open
                 closed
                 (rest nn)
                 fscores
                 dists
                 curr
                 start)))))

(defn shortest-path
  [heuristic src]
   (loop [open (priority-map src (heuristic src))
          closed #{}
          fscores (priority-map src (heuristic src))
          dists (priority-map src 0)]
     (if (should-terminate? open dists)
       (dist-to-goal dists)
       (let [curr (first (peek open))
             res (check-neighbors heuristic 
                                  (pop open) 
                                  (conj closed (clean-state curr))
                                  (neighbors curr closed)
                                  fscores 
                                  dists 
                                  curr
                                  (get-now)
                                  )]
         (recur (get res 0)
                (get res 1)
                (dissoc (get res 2) curr) ; remove curr from fscores
                (get res 3))))))

(defn heuristic1
  [state]
  (let [goal [0 0]
        data-pos (:data-pos state)]
    (reduce + (map abs (map - goal data-pos)))))

(defn start-state
  [file-name]
  (let [node-map (make-node-map (get-input file-name)) ]
  {:node-map node-map 
   :data-pos (initial-data-pos node-map)
   :possible-moves (possible-moves (vals node-map))}))

(def test-start-state (start-state "test-input.txt"))

(defn test-part-two
  []
  (shortest-path heuristic1 test-start-state))

(defn part-two
  []
;  (shortest-path heuristic1 (start-state "input.txt")))
  249) ;Trick question, solved manually


(def t0 test-start-state)

(def t1 {:node-map
  {[0 0] {:pos [0 0], :size 10, :used 8, :avail 2, :use% 80},
   [0 1] {:pos [0 1], :size 11, :used 6, :avail 5, :use% 54},
   [0 2] {:pos [0 2], :size 32, :used 28, :avail 4, :use% 87},
   [1 0] {:pos [1 0], :size 9, :used 0, :avail 9, :use% 0},
   [1 1] {:pos [1 1], :size 8, :used 7, :avail 1, :use% 88},
   [1 2] {:pos [1 2], :size 11, :used 7, :avail 4, :use% 63},
   [2 0] {:pos [2 0], :size 10, :used 6, :avail 4, :use% 60},
   [2 1] {:pos [2 1], :size 9, :used 8, :avail 1, :use% 88},
   [2 2] {:pos [2 2], :size 9, :used 6, :avail 3, :use% 66}},
  :data-pos [2 0]})

(def tx {:node-map
   {[0 0] {:pos [0 0], :size 10, :used 8, :avail 2, :use% 80},
    [0 1] {:pos [0 1], :size 11, :used 6, :avail 5, :use% 54},
    [0 2] {:pos [0 2], :size 32, :used 28, :avail 4, :use% 87},
    [1 0] {:pos [1 0], :size 9, :used 0, :avail 9, :use% 0},
    [1 1] {:pos [1 1], :size 8, :used 7, :avail 1, :use% 88},
    [1 2] {:pos [1 2], :size 11, :used 7, :avail 4, :use% 63},
    [2 0] {:pos [2 0], :size 10, :used 6, :avail 4, :use% 60},
    [2 1] {:pos [2 1], :size 9, :used 8, :avail 1, :use% 88},
    [2 2] {:pos [2 2], :size 9, :used 6, :avail 3, :use% 66}},
   :data-pos [2 0]})

(def t2  {:node-map
   {[0 0] {:pos [0 0], :size 10, :used 8, :avail 2, :use% 80},
    [0 1] {:pos [0 1], :size 11, :used 6, :avail 5, :use% 54},
    [0 2] {:pos [0 2], :size 32, :used 28, :avail 4, :use% 87},
    [1 0] {:pos [1 0], :size 9, :used 6, :avail 3, :use% 67},
    [1 1] {:pos [1 1], :size 8, :used 7, :avail 1, :use% 88},
    [1 2] {:pos [1 2], :size 11, :used 7, :avail 4, :use% 63},
    [2 0] {:pos [2 0], :size 10, :used 0, :avail 10, :use% 0},
    [2 1] {:pos [2 1], :size 9, :used 8, :avail 1, :use% 88},
    [2 2] {:pos [2 2], :size 9, :used 6, :avail 3, :use% 66}},
   :data-pos [1 0]})

