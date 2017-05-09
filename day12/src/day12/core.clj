(ns day12.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn get-lines
  [file]
  (str/split (slurp file) #"\n"))

(defn parse-line
  [line]
  (let [jnz (re-find #"jnz (\w+) (-?[1-9]\d*)" line)
        cpy-reg (re-find #"cpy ([a-d]) (\w+)" line)
        cpy-num (re-find #"cpy (-?[1-9]\d*) (\w+)" line)
        dec1 (re-find #"dec (\w+)" line)
        inc1 (re-find #"inc (\w+)" line)]
    (if (= 3 (count jnz)) {:cmd "jnz" :a (get jnz 1) :b (Integer/parseInt (get jnz 2))}
        (if (= 3 (count cpy-reg)) {:cmd "cpy-reg" :a (get cpy-reg 1) :b (get cpy-reg 2)}
            (if (= 3 (count cpy-num)) {:cmd "cpy-num" :a (Integer/parseInt (get cpy-num 1)) :b (get cpy-num 2)}
                (if (= 2 (count dec1)) {:cmd "dec" :a (get dec1 1)}
                    {:cmd "inc" :a (get inc1 1)}))))))

(defn parse-lines
  [file]
  (map parse-line (get-lines file)))

(defn initial 
  [file]
  {:cmds (parse-lines file) :pc 0 :regs {"a" 0 "b" 0 "c" 0 "d" 0}})

(defn exec-jnz
  [cmd state]
  (let [register (:a cmd)
        offset (:b cmd)
        value (get (:regs state) register) ]
        (if (= 0 value)
            (update state :pc inc)
            (update state :pc #(+ % offset)))))

(defn inc-reg
  [reg state]
  (let [new-regs (update (:regs state) reg inc)]
    (assoc state :regs new-regs :pc (+ 1 (:pc state)))))

(defn dec-reg
  [reg state]
  (let [new-regs (update (:regs state) reg dec)]
    (assoc state :regs new-regs :pc (+ 1 (:pc state)))))

(defn copy-num
  [n dest state]
  (let [new-regs (assoc (:regs state) dest n)]
    (assoc state :regs new-regs :pc (+ 1 (:pc state)))))

(defn exec-copy-num
  [cmd state]
    (copy-num (:a cmd) (:b cmd) state))

(defn copy-reg
  [src dest state]
  (let [value (get (:regs state) src)
        new-regs (assoc (:regs state) dest value)]
    (assoc state :regs new-regs :pc (+ 1 (:pc state)))))

(defn exec-copy-reg
  [cmd state]
    (copy-reg (:a cmd) (:b cmd) state))

(defn exec-inc
  [cmd state]
    (inc-reg (:a cmd) state))

(defn exec-dec
  [cmd state]
    (dec-reg (:a cmd) state))

(def funcs {"cpy-reg" "exec-copy-reg" "cpy-num" "exec-copy-num" "jnz" "exec-jnz" "inc" "exec-inc" "dec" "exec-dec"})

(defn exec-cmd
  [cmd state]
  (let [fname (:cmd cmd)
        f (get funcs fname)]
        ((resolve (symbol f)) cmd state)))

(defn exec-cmds
  [state] 
;  (pprint state)
   (if (>= (:pc state) (count (:cmds state)))
       state
       (let [pc (:pc state)
             cmd (nth (:cmds state) (:pc state))
;             p (println (:regs state))
;             sd (pprint (str "next line: " cmd))
             executed (exec-cmd cmd state)
;             ff (println (str (:regs state) " " cmd))
;             f (read-line)
             ]
       (recur executed))))

(defn exec-program
  [file]
  (exec-cmds (initial file)))

(defn part-one
  []
  (exec-program "input.txt"))

