(ns leit.tree
  (:require [leit.vector :as v]))



(defn left[xs [idx el]] (v/nth-or-nil xs (inc (* idx 2 ))  ))
(defn right[xs [idx el]] (v/nth-or-nil xs (+ (* idx 2 ) 2)  ))

(defn tree-triplet[t]
  [(v/nth-or-nil t 0) (v/nth-or-nil t 1)(v/nth-or-nil t 2)] )



(defn- -tree [xs start-node  ]
  (let [l (left xs start-node)
        r (right xs start-node)
        l-tree (if (nil? l) nil  (-tree xs l ))
        r-tree (if (nil? r) nil  (-tree  xs r ))
        start-node (second start-node)
        ]
    (cond
      (and (nil? l-tree) (nil? r-tree)) [start-node]
      (nil? l-tree)(concat [start-node] [ r-tree])
      (nil? r-tree)(concat [start-node] [ l-tree])
      :else (concat [start-node] [l-tree r-tree]))))

(defn to-tree[xs]
  (let [xs (v/with-index xs)] (-tree xs (first xs))))

(defn all-paths [t ]
  (let [[r left right] (tree-triplet t)
        l-tree (if (nil? left) nil  (map #(conj  (vec %) r)(all-paths  left )))
        r-tree (if (nil? right) nil (map #(conj  (vec %) r)(all-paths  right )))
        ]
    (cond
      (and (nil? l-tree) (nil? r-tree)) [ [r]]
      (nil? l-tree) r-tree
      (nil? r-tree) l-tree
      :else (concat l-tree r-tree))
    ))
