(ns functional_data_structures.core
  (:gen-class))

; From Chapter 2.1
(defn suffixes
  ([coll1] (suffixes coll1 []))
  ([coll1 coll2]
  (if (empty? coll1)
    (conj coll2 coll1)
    (suffixes (rest coll1) (conj coll2 coll1)))))

; from chap 2
(defn member [elem tree]
  (let [root (second tree) left (first tree) right (last tree)]
    (cond
      (empty? tree) false
      (< elem root) (member elem left)
      (> elem root) (member elem right)
      :else true)))

(defn -main [& args]
  (println (member 3 [[1 3 4] 7 5])))
