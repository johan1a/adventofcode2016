(ns day1.core
  (:gen-class))
(require '[clojure.string :as str])

(defn to-pos
  [state]
  {:xPos (:xPos state) :yPos (:yPos state)})

(def origin 
  {:direction :north
   :xPos 0
   :yPos 0
   :visited [{:xPos 0 :yPos 0} ]
   })

(def new-dir
  {:north {"R" :east
           "L"  :west}
   :east {"R" :south
          "L" :north}
   :south {"R" :west
           "L" :east}
   :west {"R" :north
          "L" :south}})

(def move-one
  {:north (fn [s] (update s :yPos #(+ 1 %)))
   :east (fn [s] (update s :xPos #(+ 1 %)))
   :south (fn [s] (update s :yPos #(- % 1)))
   :west (fn [s] (update s :xPos #(- % 1)))})

(defn mark-visited
  [state]
  (def cur-pos (to-pos state))
  (update state :visited #(conj % cur-pos)))

(defn walk
  [dir dist state]
  (if (== dist 0)
    state
    ((def moved ((move-one dir) state))
     (def marked (mark-visited moved))
     (walk dir (- dist 1) marked))))

(defn turn
  [state turn-dir]
  (update state :direction  #((new-dir %) turn-dir)))

(defn turn-and-walk
  [turn-dir dist state]
  (def turned-state (turn state turn-dir))
  (def moved-state (walk (:direction turned-state) dist turned-state))
  moved-state)

(defn move
  [state cmd]
  (turn-and-walk (:turn-dir cmd) (:dist cmd) state))

(defn navigate
  [state cmds]
   (if (empty? cmds)
     state
     (navigate (move state (first cmds)) (rest cmds))))

(def read-input-file
  (apply str (slurp "input.txt")))

(defn split-commas
  [string]
  (str/split string #"\,"))

(defn make-cmd
  [lit]
  {:turn-dir (str (first lit))
   :dist (Integer/parseInt (apply str (rest lit)))
  })

(defn make-input
  [raw-input]
  (map (comp make-cmd str/trim) (split-commas raw-input)))

(defn abs [n] (max n (- n)))

(defn total-distance
  [state]
  (+ (abs (:xPos state)) (abs (:yPos state))))

(defn follow-map
  [raw-input]
  (navigate origin (make-input raw-input)))

(defn first-duplicate2
  [checked [v & vv]]
  v)

(defn first-duplicate3
  [checked [v & vv]]
  (if (contains? checked v)
  checked
  vv
  ))

(defn first-duplicate
  [checked [v & vv]]
  (if (contains? checked v)
    v
    (first-duplicate (conj checked v) vv)))

(defn find-hq
  []
  (first-duplicate #{} (:visited (follow-map read-input-file))))

(defn dist-to-hq
  []
  (total-distance (first-duplicate #{} (:visited (follow-map read-input-file)))))

(defn -main
  [& args]
  (total-distance (follow-map read-input-file)))





















