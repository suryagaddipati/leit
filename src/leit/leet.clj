(ns leit.leet
  (:require [clojure.string :as str])
  (:require [leit.vector :as v])
  ;; (:require [leit.map :as mp])
  (:require [leit.tree :as t])
  (:require [leit.matrix :as m]))
(import 'java.util.regex.Pattern)

;util
(defn -main
  [& args]
  (println "Hello, World!"))
(defn pp [x] (do (print x) x))
(defn sum [xs] (reduce + xs))

(defn repeat-till [x n] (take-while #(<= (sum %) n) (map #(repeat % x) (range))))

(defn rotate
  ([xs] (conj (vec (rest xs)) (first xs)))
  ([xs n] (reduce (fn [xs x] (conj xs (rotate (last xs)))) [xs] (range 1 n))))


;qs-227


((fn k [x]
   (let [split (fn [x, y]  (map str/trim (clojure.string/split x (Pattern/compile y Pattern/LITERAL))))]
     (cond
       (str/includes? x "+") (reduce #(+ %1 (k %2)) 0 (split x "+"))
       (str/includes? x "*") (reduce #(* %1 (k %2))  1 (split x "*"))
       (str/includes? x "/") (reduce #(quot (k %1) (k %2))  (split x "/"))
       :else (Integer/parseInt x)))) "3 / 2 *3")

;qs-39
((fn ki [xs n]
   (if (or (= n 0) (empty? xs))
     []
     (let [fst (first xs)
           rst (rest xs)
           f-sets (repeat-till fst n)]
       (reduce  #(let [sum-set (sum %2)
                       rest-n (- n sum-set)
                       rest-k  (ki rst rest-n)
                       all-sums (map (fn [xs] (concat %2 xs)) rest-k)]
                   (concat %1  (if (= sum-set n) [%2] all-sums)))
                []  f-sets)))) [2 3 6 7] 7)


;qs-40


((fn ki [xs n]
   (let [all-sets
         (if (or (<= n 0) (empty? xs))
           #{}
           (let [fst (first xs)
                 rst (rest xs)
                 f-sets [#{} #{fst}]]
             (reduce  #(let [sum-set (sum %2)
                             rest-n (- n sum-set)
                             rest-k  (ki rst rest-n)
                             all-sums (map (fn [xs] (concat %2 xs)) rest-k)]
                         (clojure.set/union %1 (if (= sum-set n) #{%2} all-sums)))
                      #{}  f-sets)))] (reduce conj #{} all-sets))) (sort [2,5,2,1,2])  5)

;qs-238
((fn [xs]
   (let [left-sum (fn [xs] (reduce (fn [xs x]
                                     (let [lst  (if (empty? xs) [1 1] (last xs))]
                                       (conj xs [(* (first lst) (last lst)) x]))) [] xs))
         left (left-sum xs)
         right (reverse (left-sum (reverse xs)))]
     (map #(* (first %1) (first %2)) left right))) [1 2 3 4])

;qs-46
((fn k [xs]
   (if (= 1 (count xs)) [xs]
       (let [rots (rotate xs (count xs))]
         (reduce (fn [xs x]
                   (let [fst (first x)
                         rst (rest x)
                         rst-combs (k rst)
                         fcombs (map #(conj % fst) rst-combs)]
                     (concat xs fcombs)))  [] rots)))) [1,1,2])

;qs-64
((fn k
   ([xs] (k xs 0))
   ([xs n]
    (let [fst (first (first xs))
          left   (map rest xs)
          bottom  (rest xs)
          dleft #(k left (+ n fst))
          dbottom #(k bottom (+ n fst))]
      (cond
        (nil? fst) n
        (every? empty? left) (dbottom)
        (every? empty? bottom) (dleft)
        :else (if (< (dbottom) (dleft)) (dbottom) (dleft))))))
 [[1,3,1],
  [1,5,1],
  [4,2,1]])

;qs-79
((fn k
   ([xs x]
    (k xs x [[0 0]] #{}))
   ([xs x pxs visited]
    (let [next-neighbors (reduce  (fn [[js visited] j]
                                    (if (= (first x) (m/at-pos xs j))
                                      [(concat js (m/neighbors xs j visited)) (conj visited j)]
                                      [js visited]))
                                  [[] visited]
                                  pxs)]
      (cond
        (empty? x) true
        (empty? (first next-neighbors)) false
        :else  (k xs (rest x) (first next-neighbors) (second next-neighbors))))))
 [[\A \B \C \E]
  [\S \F \C \S]
  [\A \D \E \E]]
 "ABFDEX")

(defn split-half [xs] (split-at (quot (count xs) 2) xs))
(defn middle [xs] (nth  xs (quot (count xs) 2)))
(defn all= [x & xs] (every? #(= x %) xs))

;qs-35
((fn k [xs n]
   (cond
     (empty? xs) [-1 -1]
     (= 1 (count xs)) (let [[i j] (first xs)]
                        (if (= n j) [i i] [-1 -1]))
     :else (let [[lhalf rhalf] (split-half xs)
                 [mid-i mid] (middle xs)]
             (cond
               (= n mid) (let [[li _] (k lhalf n)
                               [_ rj] (k rhalf n)]
                           (cond
                             (all= -1 li rj) [mid-i mid-i]
                             (= li -1) [mid-i rj]
                             (= rj -1) [li mid-i]
                             :else [li rj]))
               (< n mid) (k lhalf n)
               (> n mid) (k rhalf n))))) (v/with-index [5,7,7,8,8,10]) 8)


;qs-807


((fn [xs]
   (let [m-maxs  (map v/max (m/rows xs))
         n-maxs  (map v/max (m/cols xs))]
     (reduce (fn [xs [[m-idx n-idx] el]]
               (let [m-max (nth m-maxs m-idx)
                     n-max (nth n-maxs n-idx)
                     mn-max (min n-max m-max)]
                 (+ xs  (- mn-max el)))) 0 (m/with-index xs))))
 [[3, 0, 8, 4],
  [2, 4, 5, 7],
  [9, 2, 6, 3],
  [0, 3, 1, 0]])

;qs-312
((fn [xs]
   (letfn [(k [xs]
             (if (= 1 (count xs))
               xs
               (let [products (map v/* (partition 3 1 (concat [1] xs [1])))
                     excepts (v/except xs)
                     k-excepts (map k excepts)
                     kk (map   (fn [k-except product]
                                 (map #(+ product %) (flatten  k-except))) k-excepts products)]
                 kk)))]
     (v/max (flatten (k xs))))) [3,1,5,8])

;qs-907
((fn [xs]
   (let [cw (reduce concat (v/windows xs))]
     (reduce #(+ %1 (v/min %2)) 0 cw))) [3,1,2,4])


;qs-457


((fn [xs]
   (letfn [(is-cycle [start path]
             (let [xs-len (count xs)
                   [idx val] start
                   next-idx  (mod  (+ idx val xs-len) xs-len)
                   next-val (nth xs next-idx)
                   path (conj path idx)]
               (if (contains? path next-idx)
                 path
                 (is-cycle next-val path))))] (is-cycle (first xs) #{}))) (v/with-index [-2,1,-1,-2,-2]))
;qs-90
((fn [xs]
   (set (reduce #(concat %1 (for [y  %1] (conj y %2)))
                #{[]} xs))) [1,2,2])

;qs-187
((fn [xs]
   (let [wins (map clojure.string/join (first (v/windows xs 10  10)))]
     (second
      (reduce #(let [[set out] %1
                     out (if (contains? set %2) (conj out %2) out)
                     set (conj set %2)] [set out])
              [#{} []]
              wins))))

 (vec "AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT"))

;qs-96
((fn k [n]
   (if (= n 0) 1
       (reduce #(+ %1 (* (k %2) (k (- n (inc %2)))))  0 (range n)))) 3)

;qs-199
((fn [xs]
   (let [xs  (v/with-index xs)]
     (loop [out []
            idx 0]
       (if (>= idx (count xs)) (map second out)
           (recur (conj out (nth xs idx)) (+ (* idx 2) 2)))))) [1,2,3,nil,5,nil,4])

;qs-332 - Incomplete

;["JFK","ATL","JFK","SFO","ATL","SFO"]
;["JFK","SFO","ATL","JFK","ATL","SFO"] <--not
((fn [xs]
   (loop [map (mp/graph xs)
          key "JFK"
          path [key]]
     (let [[nmap fst] (mp/first-get-remove map key)]
       (if (nil? fst) path (recur nmap fst (conj path fst)))))) [["JFK","SFO"],["JFK","ATL"],["SFO","ATL"],["ATL","JFK"],["ATL","SFO"]])

;qs-221 - Incomplete
((fn [xs]
   (let [sizes (reverse (for [s (range (v/min (m/size xs)))]
                          [(inc s) (inc s)]))
         all-idx (m/all-indexes xs)]
     (m/reduce-sub-matrix xs [1 2] [2 2] #(and %1 (= 1 %2)) true)))     [[1 0 1 0 0]
                                                                         [1 1 1 1 1]
                                                                         [1 1 1 1 1]
                                                                         [1 0 0 1 0]])

;qs-129
((fn [xs]
   (reduce (fn[xs x](+ xs (v/to-int  (reverse x))) )
           0
           (t/all-paths (t/to-tree  xs)))) [4,9,0,5,1] )
;qs-494
((fn k[[x & xs] n]
   (let [x+ x
         x- (* -1 x)]
     (if (empty? xs) (cond
                       (= 0  (+ n x-)) [[x]]
                       (= 0  (+ n x+ )) [[x-]]
                       :else [])
         (let [next+ (k xs (+ n x+))
               next- (k xs (+ n x-))
               next++  (map #(cons x- % ) next+)
               next--  (map #(cons x+ % ) next-)
               ]
           (concat next++ next--))))) [1, 1, 1, 1, 1] 3)

;qs-396
(defn sum[xs i]
  (let [len (count xs)]
    (reduce (fn[sum [idx val]]
              (+ sum  (* val (mod  (+ idx len i) len ))) ) 0 xs) ))
((fn[xs]
   (let [len (count xs)]
     (v/max
      (reduce (fn [ys y]
                (conj ys (sum xs y) ))  [] (range len))))) (v/with-index [4, 3, 2, 6]))

;qs-856 - Incomplete
((fn k[s stack outs last-close]
   (if (every? empty? [s stack]) outs
       (let [ out (last outs)
             f (first s)
             rst (rest s)
             lstack (last stack)]
         (cond
           (= f \( ) (k rst   (conj stack f) (if last-close (conj outs 0) outs) false)
           (= lstack  \() (k rst (butlast stack) (v/replace-last outs (if (= 0 out) 1 (* 2 out)) ) true)
           :else  (k rst   (conj stack f) outs true)
           )))) "(()(()))" [] [0] false)


;qs-1155
((fn k[dice, faces, target]
   (if (= dice 1)
     (if (and (> target 0) (<= target faces)) 1 0)
     (reduce (fn [total face] (+ total (k (dec dice) faces (- target face))))
      0 (v/range faces))
     )) 2, 6, 7 )
