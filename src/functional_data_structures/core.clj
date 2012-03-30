(ns functional_data_structures.core
  (:gen-class))

; From Chapter 2.1
(defn suffixes
  ([coll1] (suffixes coll1 []))
  ([coll1 coll2]
  (if (empty? coll1)
    (conj coll2 coll1)
    (suffixes (rest coll1) (conj coll2 coll1)))))

(defn -main [& args]
  (println (suffixes [1 2 3 4])))
