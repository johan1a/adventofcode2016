(ns day14.core
  (:gen-class))
  (require '[clojure.string :as str])
(import 'java.security.MessageDigest
                'java.math.BigInteger)
(:define pprint)

;; https://gist.github.com/jizhang/4325757
(defn md5 [s]
    (let [algorithm (MessageDigest/getInstance "MD5")
                  raw (.digest algorithm (.getBytes s))]
          (format "%032x" (BigInteger. 1 raw))))

(def puzzle-salt "ahsbgdzn")
(def test-salt "abc")

(defn all-same?
  [string]
  (= 1 (count (set string))))

(defn is-triplet?
  [string]
  (and (= 3 (count string))
       (all-same? string)))

(defn get-triplet
  [string]
  (if (= 0 (count string))
      false
      (let [three (take 3 string)]
        (if (is-triplet? three)
            (str/join three)
            (recur (drop 1 string))))))

(defn all-equals?
  [string l]
  (and (all-same? string)
       (= l (first string))))

(defn is-5plet?
  [string l]
  (and (= 5 (count string))
       (all-equals? string l)))

(defn has-five?
  [string l]
  (if (= 0 (count string))
      false
      (let [five (take 5 string)]
        (if (is-5plet? five l)
            true
            (recur (drop 1 string) l)))))

(defn is-valid?
  [hash1 pending index]
  (let [l (:letter pending)
        has-five (has-five? hash1 l)]
    (has-five? hash1 l)))

(defn fresh?
  [pending n]
;  (pprint (str "i: " (:index pending) "n + 1000: " (+ 1000 n)))
  (and (not (false? pending))
       (<= n (+ 1000 (:index pending)))))

(defn get-pending
  [hash1 n]
  (let [triplet (get-triplet hash1)]
    (if triplet
        {:letter (first triplet) :hash hash1 :index n}
        false)))

(defn make-valid
  [valid matching matching-i]
  (assoc (assoc valid :matching-hash matching) :matching-i matching-i))

(defn find-keys
  [n pendings valids salt max1]
  (if (= 0 (mod n 100))
    (pprint (str "n: " n ", pendings: " 
                 (count pendings) ", valids: " (count valids))))
  (if (>= (count valids) max1)
      (sort-by :index valids)
      (let [hash1 (md5 (str salt n))
            new-pending (get-pending hash1 n)
            pendings2 (filter #(fresh? % n) pendings)
            new-valids (map #(make-valid % hash1 n) (filter #(is-valid? hash1 % n) pendings2))
            all-valids  (set (concat valids new-valids))
            pendings3 (filter #(not (contains? all-valids %)) (conj pendings2 new-pending))
            ]
        (recur (inc n) pendings3 all-valids salt max1))))

(defn part-one
  []
  (nth (find-keys 0 #{} #{} puzzle-salt 64) 63))

(defn test1
  []
  (find-keys 0 #{} #{} test-salt 64))


