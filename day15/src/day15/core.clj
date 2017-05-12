(ns day15.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def start
  {:discs
   [{:n 5 :curr 2}
    {:n 13 :curr 7}
    {:n 17 :curr 10}
    {:n 3 :curr 2}
    {:n 19 :curr 9}
    {:n 7 :curr 0}]
   :ball {:falling false :curr -1}
   :time 0 })

(def start2 (update start :discs #(conj % {:n 11 :curr 0})))

(def test-start
  {:discs
   [{:n 5 :curr 4}
    {:n 2 :curr 1}]
   :ball {:falling false :curr -1}
   :time 0 })

(defn next-disc-state
  [disc n]
  (assoc disc :curr (mod (+ n (:curr disc)) (:n disc))))

(defn fall
  [ball n]
  (if (:falling ball)
      (update ball :curr #(+ n %))
      ball))

(defn at-goal?
  [state]
  (= (count (:discs state)) (:curr (:ball state))))

(defn failed?
  [state]
  (if (>= (:curr (:ball state)) 0)
      (let [disc (nth (:discs state) (:curr (:ball state)))]
        (> (:curr disc) 0))
      false))

(defn wait
  [state n]
  {:discs (map #(next-disc-state % n) (:discs state))
   :time (+ n (:time state))
   :ball (fall (:ball state) n)})

(defn simulate
  [state]
  (if (at-goal? state) true
    (if (failed? state) false
        (recur (wait state 1)))))

(defn drop-ball
  [state]
  (let [ball (:ball state)]
    (assoc state :ball (assoc ball :falling true))))

(defn will-finish?
  [state]
  (simulate (drop-ball state)))

(defn find-start-time
  ([state] (find-start-time state 0))
  ([state n]
  (if (will-finish? state) n
      (recur (wait state 1) (inc n)))))

(defn part-one
  []
  (find-start-time start))

(defn part-two
  []
  (find-start-time start2))




