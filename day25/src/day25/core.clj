(ns day25.core
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
        out  (re-find #"out (\w+)" line)
        ]
    (cond
      (= 3 (count jnz))      { :cmd "jnz" :a (get jnz 1) :b (get jnz 2)}
      (= 3 (count cpy))      { :cmd "cpy" :a (get cpy 1) :b (get cpy 2)}
      (= 3 (count mul))      { :cmd "mul" :a (get mul 1) :b (get mul 2)}
      (= 2 (count dec1))     { :cmd "dec" :a (get dec1 1)}
      (= 2 (count tgl))      { :cmd "tgl" :a (get tgl 1)}
      (= 2 (count inc1))     { :cmd "inc" :a (get inc1 1)}
      (= 4 (count noop))     { :cmd "noop"}
      (= 2 (count out))      { :cmd "out" :a (get out 1)}
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

(defn output
  [state amount]
  (let [new-out (conj (:out state) amount)]
    (println new-out)
    (assoc state :out new-out :pc (+ 1 (:pc state)))))

(defn exec-out
  [cmd state]
    (if (not (valid-register? (:a cmd)))
        (output state (:a cmd)))
        (output state (get (:regs state) (:a cmd))))

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
            "mul" "exec-mul"
            "out" "exec-out"})

(defn exec-cmd
  [cmd state]
  (let [fname (:cmd cmd)
        f (get funcs fname)]
        ((resolve (symbol f)) cmd state)))

(defn good-sequence
  [state]
  (let [out (:out state)]
    (or (= out [])
        (= out [1])
        (= out [0])
        (= out [1 0])
        (= out [0 1])
      (let [lastn (last out)
            next-last (last (butlast out))
            next-next-last (last (butlast (butlast out)))
]
      (or
        (and
          (= 1 lastn)
          (= 0 next-last)
          (= 1 next-next-last))
        (and
          (= 0 lastn)
          (= 1 next-last)
          (= 0 next-next-last)))))))

(defn inc-a
  [state]
  (let [a-reg (get (:regs state) "a")]
    (println (str "increasing a to " (inc a-reg)))
    (assoc (inc-reg "a" state) :pc 0)))

(defn handle-print
  [state c ]
  (let [m (mod c 1000000)]
    (if (= 0 m)
      (println (:regs state)))))

(defn exec-cmds
  ([original-state state c]
   (if
       (>= (:pc state) (count (:cmds state)))
           (let [increased (inc-a original-state)]
                (recur increased increased 0))
           (let [pc (:pc state)
                 cmd (nth (:cmds state) (:pc state))
                 executed (exec-cmd cmd state)
                 not-good (not (good-sequence executed))
                 ]
             (if (or not-good false)
                 (let [increased (inc-a original-state)]
                   (recur increased increased 0))
                   (recur original-state executed (inc c)))))))

(defn initial
  [file n ]
  {:cmds (parse-lines file) :pc 0 :regs {"a" n "b" 0 "c" 0 "d" 0} :out []})

(defn run
  [n]
  (let [i (initial "input.txt" n)]
    (exec-cmds i i 0)))

