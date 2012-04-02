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
; this implementation of member takes 2 * depth comparisions
(defn slow-member [elem tree]
  (let [root (second tree) left (first tree) right (last tree)]
    (cond
      (empty? tree) false
      (< elem root) (slow-member elem left)
      (> elem root) (slow-member elem right)
      :else true)))

; problem 2.3
; this implementation of member takes depth + 1 comparisions
(defn fast-member
  ([elem tree] (fast-member elem tree nil))
  ([elem tree prev]
   (let [root (second tree) left (first tree) right (last tree)]
     (cond
       (and (nil? left) (nil? right)) (or (= root elem) (= prev elem))
       (< elem root) (fast-member elem left root)
       :else (fast-member elem right root)))))

(defn -main [& args]
  (println (fast-member 5 [[nil 2 nil] 3 [nil 4 nil]])))
