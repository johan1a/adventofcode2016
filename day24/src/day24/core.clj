(ns day24.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer :all]
            [day24.path-finding :as pf]))
(use '[clojure.pprint :only (pprint)])

(defn part-one
  []
  (pf/find-distances "input.txt"))

