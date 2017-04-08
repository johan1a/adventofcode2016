(ns day1.core
  (:gen-class))
(require '[clojure.string :as str])

(def origin 
  {:direction :north
   :xDist 0
   :yDist 0})

(def new-dir
  {:north {"R" :east
           "L"  :west}
   :east {"R" :south
          "L" :north}
   :south {"R" :west
           "L" :east}
   :west {"R" :north
          "L" :south}})

(def walk
  {:north (fn [d s] (update s :yDist #(+ d %)))
   :east (fn [d s] (update s :xDist #(+ d %)))
   :south (fn [d s] (update s :yDist #(- % d)))
   :west (fn [d s] (update s :xDist #(- % d)))})

(defn turn
  [state turn-dir]
  (update state :direction  #((new-dir %) turn-dir)))

(defn turn-and-walk
  [turn-dir dist state]
  (def turned-state (turn state turn-dir))
  ((walk (:direction turned-state)) dist turned-state))

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
  (+ (abs (:xDist state)) (abs (:yDist state))))

(defn follow-map
  [raw-input]
  (navigate origin (make-input raw-input)))

(defn -main
  [& args]
  (total-distance (follow-map read-input-file)))





















