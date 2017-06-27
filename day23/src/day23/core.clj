(ns day23.core
  (:gen-class)
  (:require [clojure.string :as str])
  (:use clojure.pprint))

(defn get-lines
  [file]
  (str/split (slurp file) #"\n"))

(defn parse-line
  [line]
  (let [jnz  (re-find #"jnz (\w+) (-?\w+)" line)
        cpy  (re-find #"cpy (-?\w+) (\w+)" line)
        mul  (re-find #"mul (\w+) (\w+)" line)
        dec1 (re-find #"dec (-?\w+)" line)
        tgl  (re-find #"tgl (\w+)" line)
        inc1 (re-find #"inc (\w+)" line)
        noop (re-find #"noop" line)
        ]
    (cond
      (= 3 (count jnz))      { :cmd "jnz" :a (get jnz 1) :b (get jnz 2)}
      (= 3 (count cpy))      { :cmd "cpy" :a (get cpy 1) :b (get cpy 2)}
      (= 3 (count mul))      { :cmd "mul" :a (get mul 1) :b (get mul 2)}
      (= 2 (count dec1))     { :cmd "dec" :a (get dec1 1)}
      (= 2 (count tgl))      { :cmd "tgl" :a (get tgl 1)}
      (= 2 (count inc1))     { :cmd "inc" :a (get inc1 1)}
      (= 4 (count noop))     { :cmd "noop"}
      :else                  { :cmd "error" :a line})))

(defn parse-lines
  [file]
  (map parse-line (get-lines file)))

(defn valid-register?
  [x]
  (str/includes? "abcd" x))

(defn inc-pc
  [state]
  (update state :pc inc))

(defn is-number?
  [string]
  (re-find #"-?[0-9]\d*" string))

(defn regval-or-num
  [state x]
  (cond
    (valid-register? x) (get (:regs state) x)
    (is-number? x) (Integer/parseInt x)
    :else false))

(defn exec-jnz
  [cmd state]
  (let [a (:a cmd)
        b (:b cmd)]
        (let [value (regval-or-num state a)
              offset (regval-or-num state b)]
          (if (or (not value) (not offset)) 
            (inc-pc state)
            (if (= 0 value)
              (inc-pc state)
              (update state :pc #(+ % offset)))))))

(defn inc-reg
  [reg state]
  (if (not (valid-register? reg)) (inc-pc state))
  (let [new-regs (update (:regs state) reg inc)]
    (assoc state :regs new-regs :pc (+ 1 (:pc state)))))

(defn dec-reg
  [reg state]
  (if (not (valid-register? reg)) 
    (inc-pc state)
    (let [new-regs (update (:regs state) reg dec)]
      (assoc state :regs new-regs :pc (+ 1 (:pc state))))))

(defn convert-cmd
  [old-cmd]
  (let [cmd-name (:cmd old-cmd)]
    (cond
      (= "jnz" cmd-name)     (assoc old-cmd :cmd "cpy" :toggled true)
      (= "cpy" cmd-name)     (assoc old-cmd :cmd "jnz" :toggled true)
      (= "dec" cmd-name)     (assoc old-cmd :cmd "inc" :toggled true)
      (= "tgl" cmd-name)     (assoc old-cmd :cmd "inc" :toggled true)
      (= "inc" cmd-name)     (assoc old-cmd :cmd "dec" :toggled true))))

(defn out-of-range?
  [state offset]
  (let [pc (:pc state)
        total (+ pc offset)
        cmd-count (count (:cmds state))]
    (or (< total 0)
        (>= total cmd-count))))

(defn toggle-cmd
  [offset state]
  (if (out-of-range? state offset)
    (inc-pc state)
    (let [old-pc (:pc state)
          old-cmds (vec (:cmds state))
          cmd-index (+ offset old-pc)
          old-cmd (nth (:cmds state) cmd-index)
          already-toggled (:toggled old-cmd)]
        (if already-toggled
          (inc-pc state)
          (let [ new-cmd (convert-cmd old-cmd)
                new-cmds (assoc old-cmds cmd-index new-cmd)]
              (assoc state :cmds new-cmds :pc (+ 1 (:pc state))))))))

(defn copy-num
  [n dest state]
  (let [new-regs (assoc (:regs state) dest n)]
    (assoc state :regs new-regs :pc (+ 1 (:pc state)))))

(defn copy-reg
  [src dest state]
  (let [value (get (:regs state) src)
        new-regs (assoc (:regs state) dest value)]
    (assoc state :regs new-regs :pc (+ 1 (:pc state)))))

(defn exec-copy
  [cmd state]
  (let [a (:a cmd)
        b (:b cmd)]
    (if (valid-register? b)
        (if (valid-register? a)
          (copy-reg a b state)
          (copy-num (Integer/parseInt a) b state))
        (inc-pc state))))

(defn exec-mul
  "mul a and b and store the result in a"
  [cmd state]
  (let [a-reg (:a cmd)
        a-val (regval-or-num state a-reg)
        b (regval-or-num state (:b cmd))
        result (* a-val b)
        new-regs (assoc (:regs state) a-reg result)]
    (assoc state :regs new-regs :pc (+ 1 (:pc state)))))

(defn exec-inc
  [cmd state]
    (if (not (valid-register? (:a cmd))) 
      (inc-pc state))
      (inc-reg (:a cmd) state))

(defn exec-dec
  [cmd state]
    (if (not (valid-register? (:a cmd)))
        (inc-pc state))
        (dec-reg (:a cmd) state))

(defn get-reg
  [state reg]
  (get (:regs state) reg))

(defn exec-tgl
  [cmd state]
  (let [a (:a cmd)
        regs (:regs state)]
    (if (valid-register? a)
        (toggle-cmd (get-reg state a) state)
        (if (is-number? a)
            (toggle-cmd a state)
            (inc-pc state)))))

(defn exec-noop
  [cmd state]
  (inc-pc state))

(def funcs {"cpy" "exec-copy" 
            "jnz" "exec-jnz" 
            "inc" "exec-inc" 
            "dec" "exec-dec"
            "tgl" "exec-tgl"
            "noop" "exec-noop"
            "mul" "exec-mul"})
 
(defn exec-cmd
  [cmd state]
  (let [fname (:cmd cmd)
        f (get funcs fname)]
        ((resolve (symbol f)) cmd state)))

(defn exec-cmds
  [state] 
;  (if (or (= 17 (:pc state)) 
;          (= 10 (:pc state)))
;    (pprint state))
   (if 
       (>= (:pc state) (count (:cmds state)))
       state
       (let [pc (:pc state)
             cmd (nth (:cmds state) (:pc state))
             executed (exec-cmd cmd state)
;             ff (println (str (:regs state) " " (:pc state) " " cmd))
             ]
       (recur executed))))

(defn exec-program
  [state]
  (exec-cmds state))

(defn test-initial 
  [file]
  {:cmds (parse-lines file) :pc 0 :regs {"a" 0 "b" 0 "c" 0 "d" 0}})

(defn initial 
  [file]
  {:cmds (parse-lines file) :pc 0 :regs {"a" 7 "b" 0 "c" 0 "d" 0}})

(defn initial-part2
  [file]
  {:cmds (parse-lines file) :pc 0 :regs {"a" 12 "b" 0 "c" 0 "d" 0}})

(defn test-part1
  []
  (exec-cmds (test-initial "test-input.txt")))

(defn run-part1
  []
  (exec-cmds (initial "input.txt")))

(defn part1
  []
  (get (:regs (run-part1)) "a"))

(defn test-part2
  []
  (exec-cmds (initial "input2.txt")))

(defn run-part2
  []
  (exec-cmds (initial-part2 "input2.txt")))

(defn part2
  []
  (get (:regs (run-part2)) "a"))


