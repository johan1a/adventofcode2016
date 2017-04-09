(ns day2.core
  (:gen-class))
(require '[clojure.string :as str])

(def origin {:pos 5
             :code '()})

(def get-new-pos
  {1 {"U" 1  "D" 4 "L" 1 "R" 2}
   2 {"U" 2  "D" 5 "L" 1 "R" 3}
   3 {"U" 3  "D" 6 "L" 2 "R" 3}
   4 {"U" 1  "D" 7 "L" 4 "R" 5}
   5 {"U" 2  "D" 8 "L" 4 "R" 6}
   6 {"U" 3  "D" 9 "L" 5 "R" 6}
   7 {"U" 4  "D" 7 "L" 7 "R" 8}
   8 {"U" 5  "D" 8 "L" 7 "R" 9}
   9 {"U" 6  "D" 9 "L" 8 "R" 9}})

(defn move
  [state cmd]
  (update state :pos #((get-new-pos %) cmd)))

(defn follow-line
  [state cmds]
  (if (empty? cmds)
    (update state :code #(conj % (:pos state)))
    (follow-line (move state (str (first cmds))) (rest cmds))))

(defn follow-lines
  [state lines]
  (if (empty? lines)
  state
  (let [new-state (follow-line state (first lines))]
  (follow-lines new-state (rest lines)))))

(defn read-input-file
  [file-name]
  (apply str (slurp file-name)))

(defn split-lines
  [string]
  (str/split string #"\n"))

(defn get-input
  [file-name]
  (split-lines (read-input-file file-name)))

(defn find-code
  [input-file]
  (reverse (:code (follow-lines origin (get-input input-file)))))

(defn -main
  [& args]
  (find-code "input.txt"))











