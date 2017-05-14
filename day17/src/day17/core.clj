(ns day17.core
  (:gen-class) 
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer :all] 
            ))
(import 'java.security.MessageDigest)
(use '[clojure.pprint :only (pprint)])

(def password "pslxynzg")
(def start { :pos [0 0] :path password })

(def t0 {:pos [0 0] :path "ihgpwlah" })
(def t1 {:pos [0 0] :path "kglvqrro" })
(def t2 {:pos [0 0] :path "ulqzkmiv" })


(def goal { :pos [3 3] })

;; https://gist.github.com/jizhang/4325757
(defn md5 [s]
    (let [algorithm (MessageDigest/getInstance "MD5")
                  raw (.digest algorithm (.getBytes s))]
          (format "%032x" (BigInteger. 1 raw))))

(def start2 { :pos [0 0] :hash (md5 password) })

(def dirs [[0 -1]
           [0 1] 
           [-1 0] 
           [1 0]])

(def letter-diffs
  {[0 -1] \U
   [0 1]  \D
   [-1 0] \L
   [1 0]  \R})

(def dir-letters [\U \D \L \R])

(def valid-chars #{\b \c \d \e \f})

(defn make-offset
  [chars- i dir]
  (let [c (nth chars- i)]
    (if (contains? valid-chars c)
        {:dir dir :letter (nth dir-letters i)}
        false)))

(defn get-hash-offsets
  [hash-]
  (let [chars- (take 4 hash-)]
    (filter #(not (false? %)) 
            (map-indexed (fn [i dir] (make-offset chars- i dir)) dirs))))

(defn get-offsets
  [state]
  (let [hash- (md5 (:path state))]
        (get-hash-offsets hash-)))

(defn abs
  [x]
  (if (> x 0) x (* x -1)))

(defn add
  [pos dir]
  (map + pos dir))

(defn sub-state
  [a b]
  (map - (:pos a) (:pos b)))

(defn make-state
  [state offset]
  {:pos (add (:pos state) (:dir offset)) 
   :path (str (:path state) (:letter offset))})

(defn valid-state?
  [state]
  (let [pos (:pos state)]
    (let [x (first pos)
          y (second pos)]
      (and (>= x 0) 
           (>= y 0)
           (< x 4)
           (< y 4)))))

(defn neighbors
  ([state] (neighbors state #{}))
  ([state closed]
  (let [offsets (get-offsets state)]
    (filter #(not (contains? closed %)) 
            (filter #(valid-state? %) (map #(make-state state %) offsets))))))

(defn letter-diff
  [a b]
  (let [diff (sub-state a b)]
    (get letter-diffs diff) ))

(defn get-hash
  [state prevs string]
  (let [prev (get prevs state)]
   (if (not prev) 
     (md5 (str password string))
     (recur prev prevs (str (letter-diff state prev) string)))))
  
(def get-hash-m (memoize get-hash))

(defn make-state2
  [state offset hash-]
  {:pos (add (:pos state) (:dir offset))
   :hash hash- })

(defn get-dist
  [dists k default]
   (let [dist (get dists k)]
     (if dist dist default)))

(defn backtrack
  ([prevs curr] (backtrack prevs curr []))
  ([prevs curr path]
   (let [prev (get prevs curr)]
     (if prev
       (recur prevs prev (cons curr path))
       path))))

(defn should-terminate?
  [open dists goal]
      (= 0 (count open)))

(defn find-by-pos
  [m v]
  (first (first (filter #(= (:pos (first %)) (:pos v)) m))))
    

(defn check-neighbors
  [heuristic open closed nn fscores dists prevs curr goal comp-func default-dist]
    (if (= 0 (count nn))
      [open closed fscores dists prevs]
      (let [v (first nn)
            tentative (+ 1 (get dists curr))
            dist-v (get-dist dists v default-dist)]
        (if (comp tentative dist-v)
            (recur heuristic
                   (assoc open v (+ tentative (heuristic v goal)))
                   closed
                   (rest nn)
                   (assoc fscores v (+ tentative (heuristic v goal)))
                   (assoc dists v tentative)
                   (assoc prevs v curr)
                   curr
                   goal
                   comp-func
                   default-dist)
          (recur heuristic 
                 open
                 closed
                 (rest nn)
                 fscores
                 dists
                 prevs
                 curr
                 goal
                 comp-func
                 default-dist)))))

(defn shortest-path
  [heuristic src goal]
   (loop [open (priority-map src (heuristic src goal))
          closed #{}
          fscores (priority-map src (heuristic src goal))
          dists (priority-map src 0 goal Integer/MAX_VALUE)
          prevs {}]
     (if (should-terminate? open dists goal)
       (find-by-pos open goal)
       (let [curr (first (peek open))
             res (check-neighbors heuristic 
                                  (pop open) 
                                  (conj closed curr) 
                                  (neighbors curr closed) 
                                  fscores 
                                  dists 
                                  prevs
                                  curr
                                  goal
                                  (fn [a b] (< a b))
                                  Integer/MAX_VALUE) ]
         (recur (get res 0)
                (get res 1)
                (dissoc (get res 2) curr) ; remove curr from fscores
                (get res 3)
                (get res 4))))))

(defn get-path
  [paths prevs state]
  (let [p (get paths state)]
    (if p p 
      (let [ prev (get prevs state)
        prev-path (get paths prev)]
        (conj prev-path (sub-state state prev))))))

(defn string-path
  [diffs]
  (let [a (str/join (map #(get letter-diffs %) diffs))]
  (str password a)))

(defn neighbors2
  [state closed prevs paths]
  (let [path (string-path (get-path paths prevs state))
        hash1 (md5 path)
        offsets (get-hash-offsets hash1) ]
    (filter #(not (contains? closed %)) 
            (filter #(valid-state? %) (map #(make-state2 state % hash1) offsets)))))


(defn longest-path
  [heuristic src goal]
   (loop [open (priority-map-by > src (heuristic src goal))
          closed #{}
          fscores (priority-map-by > src (heuristic src goal))
          dists (priority-map-by > src 0 goal Integer/MIN_VALUE)
          prevs {}
          paths {src []} ]
     (let [c (count closed)]
       (if (= 0 (mod c 1000)) (pprint c)))
     (if (should-terminate? open dists goal)
       (find-by-pos open goal)
       (let [curr (first (peek open))
             res (check-neighbors heuristic 
                                  (pop open) 
                                  (conj closed curr) 
                                  (neighbors2 curr closed prevs paths) 
                                  fscores 
                                  dists 
                                  prevs
                                  curr
                                  goal
                                  (fn [a b] (> a b))
                                  Integer/MIN_VALUE) ]
         (recur (get res 0)
                (get res 1)
                (dissoc (get res 2) curr) ; remove curr from fscores
                (get res 3)
                (get res 4)
                (assoc paths curr (get-path paths prevs curr)))))))

(def start2 { :pos [0 0] })

(defn heuristic1
  [a b]
  (reduce + (map abs (sub-state b a))))

(defn solve
  [start-state]
  (subs (:path (shortest-path heuristic1 start-state goal)) (count (:path start-state))))

(defn test0
  []
  (solve t0))
  
(defn part-one
  []
  (solve start))

(defn solve-longest
  ([start-state]
   (longest-path heuristic1 start-state goal)))

(defn part-two
  []
  (solve-longest start2))

