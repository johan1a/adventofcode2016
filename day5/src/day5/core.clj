(ns day5.core
  (:gen-class))
(require '[clojure.string :as str])
(import 'java.security.MessageDigest
                'java.math.BigInteger)

;; https://gist.github.com/jizhang/4325757
(defn md5 [s]
    (let [algorithm (MessageDigest/getInstance "MD5")
                  raw (.digest algorithm (.getBytes s))]
          (format "%032x" (BigInteger. 1 raw))))

(def door-id "abbhdwsy")

(defn get-hash
  [s n]
  (md5 (str s n)))

(defn next-letter [s k]
  (let [h (get-hash s k)]
   (if (str/starts-with? h "00000")
    {:index k :letter (nth h 5)}
    (recur s (inc k)))))

(defn get-password [s n i]
    (if (= 0 n)
      ""
      (let [l (next-letter s i)]
      (str (:letter l) 
            (get-password s (dec n) (inc (:index l)))))))

(defn door-passord
  []
  (get-password door-id 8 0))



