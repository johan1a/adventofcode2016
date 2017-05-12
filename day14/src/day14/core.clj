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

(defn stretch
  [n string]
  (if (= 0 n)
      string
      (recur (dec n) (md5 string))))

(defn find-keys
  ([salt hash-func] (find-keys salt hash-func 0 64 #{} #{}))
  ([salt hash-func n max1 pendings valids]
  (if (= 0 (mod n 100))
    (pprint (str "n: " n ", pendings: " 
                 (count pendings) ", valids: " (count valids))))
  (if (>= (count valids) max1)
      (sort-by :index valids)
      (let [hash1 (hash-func (str salt n))
            new-pending (get-pending hash1 n)
            pendings2 (filter #(fresh? % n) pendings)
            new-valids (map #(make-valid % hash1 n) (filter #(is-valid? hash1 % n) pendings2))
            all-valids  (set (concat valids new-valids))
            pendings3 (filter #(not (contains? all-valids %)) (conj pendings2 new-pending))
            ]
        (recur salt hash-func (inc n) max1 pendings3 all-valids)))))

(defn part-one
  []
  (nth (find-keys puzzle-salt md5) 63))

(defn part-two
  []
  (nth (find-keys puzzle-salt (partial stretch 2017)) 63))

(defn test-one
  []
  (nth (find-keys test-salt md5) 63))

(defn test-two
  []
  (nth (find-keys test-salt (partial stretch 2017)) 63))


