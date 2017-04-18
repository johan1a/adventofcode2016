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

(defn good-hash? [h]
  (and (str/starts-with? h "00000") 
       (Character/isDigit (nth h 5))))

(defn make-obj [h i]
  {:index i :pos (Integer/parseInt (str (nth h 5))) :letter (nth h 6) :hash h})

(defn free-pos [f pos]
  (not (contains? (set (map :pos f)) pos)))

(defn next-letter2 [s i f]
  (let [h (get-hash s i)]
   (if (and (good-hash? h))
     (let [x (make-obj h i)
           pos (:pos x)]
     (if (and (>= pos 0) (< pos 8) (free-pos f pos))
         x
         (recur s (inc i) f)))
     (recur s (inc i) f))))

(defn get-password-unordered-map [s n i f]
    (if (= 0 n)
      []
      (let [l (next-letter2 s i f)
            prev-found (conj f l)]
        (println (str "found hash: " l))
        (conj (get-password-unordered-map s (dec n) (inc (:index l)) prev-found) l))))

(defn get-password-unordered [s n i]
  (let [p (get-password-unordered-map s n i [])
       x (map (fn [a] [(:pos a) (:letter a)]) p)]
   (str/join (map second (sort x)))))

(defn door-password2
  []
  (get-password-unordered door-id 8 0))

(defn door-passord
  []
  (get-password door-id 8 0))



