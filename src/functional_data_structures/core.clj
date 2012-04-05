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

; from figure 2.9
(defn insert [elem tree]
  (let [root (second tree) left (first tree) right (last tree)]
    (cond
      (empty? tree) [nil elem nil]
      (< elem root) [(insert elem left) root right]
      (> elem root) [left root (insert elem right)]
      :else elem)))

; helper for heap-merge
(defn rank [node]
  (if (nil? node)
    0
    (first node)))

; helper for heap-merge
(defn makeT [node]
  (let [root (first node) left (second node) right (last node)]
    (if (>= (rank left) (rank right))
      [(+ 1 (rank right)) root left right]
      [(+ 1 (rank left)) root right left])))

; from chap 3.1
(defn heap-merge [h1 h2]
  (let [rank1 (first h1)
        root1 (second h1)
        left1 (nth h1 2)
        right1 (last h1)
        rank2 (first h2)
        root2 (second h2)
        left2 (nth h2 2)
        right2 (last h2)]
        (cond
          (nil? h1) h2
          (nil? h2) h1
          (<= root1 root2) (makeT [root1 left1 (heap-merge right1 h2)])
          :else (makeT [root2 left2 (heap-merge h1 right2)]))))

(defn -main [& args]
  (println (heap-merge [1 3 nil nil] [1 4 nil nil])))
