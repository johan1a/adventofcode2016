(ns day10.core
  (:gen-class))
(require '[clojure.string :as str])

(defn get-input
  []
  (let [lines (str/split (slurp "input.txt") #"\n")
        val-lines (filter #(.contains % "value" ) lines)
        give-lines (filter #(not (.contains % "value" )) lines)]
  [val-lines give-lines]))

(defn initial-bots
  []
  {})

(def wanted '(17 61))

(defn check-for-wanted
  [bot new-vals]
  (if (= wanted new-vals)
    (println (str "Found bot: " bot))))

(defn put-val
  [bots bot value]
  (let [vals (get bots bot)
        new-vals (conj vals value)]
    (check-for-wanted bot new-vals)
    (assoc bots bot new-vals)))

(defn clear-vals
  [bots bot]
  (assoc bots bot '()))

(defn has-two-vals
  [bot]
  (= 2 (count bot)))

(defn propagate
  [bots cmds bot]
  (if (has-two-vals (get bots bot))
    (exec-give-cmd bots cmds bot )
    bots))

(defn exec-give-cmd
  [bots cmds src]
  (let [cmd (get cmds src)
        bot-vals (get bots src)
        low (apply min bot-vals)
        hi (apply max bot-vals)
        low-dest (:low-dest cmd)
        hi-dest (:hi-dest cmd)
        s1 (put-val bots low-dest low)
        s2 (put-val s1 hi-dest hi)
        s3 (clear-vals s2 src)
        s4 (propagate s3 cmds low-dest)
        s5 (propagate s4 cmds hi-dest)]
    s5))

(defn exec-val-cmd
  [bots line]
  (let [m (re-matches #"value (\d+) goes to (bot \d+)" line)
        bot (nth m 2)
        value (Integer/parseInt (nth m 1))]
    (put-val bots bot value)))

(defn find-has-two
  [bots]
  (let [bot (first (filter (fn [k] (> (count (second k)) 1)) bots))]
    (first bot)))

(defn exec-give-cmds
  [bots cmds]
  (let [bot (find-has-two bots)]
    (exec-give-cmd bots cmds bot)))

(defn exec-value-cmds
  [bots lines]
  (if (= 0 (count lines))
    bots
    (let [s2 (exec-val-cmd bots (first lines))]
  (recur s2 (drop 1 lines)))))

(defn parse-give-cmd
  [line]
  (let [m (re-matches #"bot (\d+) gives low to (\w+ \d+) and high to (\w+ \d+)" line)
        src (str "bot " (nth m 1))
        low-dest (nth m 2)
        hi-dest (nth m 3)]
    {src {:low-dest low-dest :hi-dest hi-dest }}))

(defn get-give-cmds
  [input]
  (let [cmds (second input)]
    (apply merge (map parse-give-cmd cmds))))

(defn get-value-cmds
  [input]
  (first input))

(defn part-1
  []
  (let [input (get-input)
        give-cmds (get-give-cmds input)
        value-cmds (get-value-cmds input)
        s1 (exec-value-cmds (initial-bots) value-cmds)
        s2 (exec-give-cmds s1 give-cmds)]))





