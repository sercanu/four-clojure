(ns four-clojure.core
  (:require proto-repl.saved-values
            clojure.set)
  (:gen-class))

;; Nothing but the Truth
;; http://www.4clojure.com/problem/1
(= true true)

;; Simple Math
;; http://www.4clojure.com/problem/2
(= (- 10 (* 2 3)) 4)

;; Intro to Strings
;; http://www.4clojure.com/problem/3
(= "HELLO WORLD" (.toUpperCase "hello world"))

;; Intro to Lists
;; http://www.4clojure.com/problem/4
(= (list :a :b :c '(:a :b :c)))

;: Lists: conj
;; http://www.4clojure.com/problem/5
(= '(1 2 3 4) (conj '(2 3 4) 1))
(= '(1 2 3 4) (conj '(3 4) 2 1))

;; Intro to Vectors
;; http://www.4clojure.com/problem/6
(= [:a :b :c] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))

;; Vectors: conj
;; http://www.4clojure.com/problem/7
(= [1 2 3 4] (conj [1 2 3] 4))
(= [1 2 3 4] (conj [1 2] 3 4))

;; Intro to Sets
;; http://www.4clojure.com/problem/8
(= #{:a :b :c :d} (set '(:a :a :b :c :c :c :c :d :d)))
(= #{:a :b :c :d} (clojure.set/union #{:a :b :c} #{:b :c :d}))

;; Sets: conj
;; http://www.4clojure.com/problem/9
(= #{1 2 3 4} (conj #{1 4 3} 2))

;; Intro to Maps
;; http://www.4clojure.com/problem/10
(= 20 ((hash-map :a 10, :b 20, :c 30) :b))
  ;; Returns a new hash map with supplied mappings.  If any keys are
  ;; equal, they are handled as if by repeated uses of assoc.
(= 20 (:b {:a 10, :b 20, :c 30}))

;; Maps: conj
;; http://www.4clojure.com/problem/11
(= {:a 1, :b 2, :c 3} (conj {:a 1} [:b 2] [:c 3]))

;; Intro to Sequences
;; http://www.4clojure.com/problem/12
(= 3 (first '(3 2 1)))
(= 3 (second [2 3 4]))
(= 3 (last (list 1 2 3)))

;; Sequences: rest
;; http://www.4clojure.com/problem/13
(= [20 30 40] (rest [10 20 30 40]))

;; Intro to Functions
;; http://www.4clojure.com/problem/14
(= 8 ((fn add-five [x] (+ x 5)) 3))
(= 8 ((fn [x] (+ x 5)) 3))
(= 8 (#(+ % 5) 3))
(= 8 ((partial + 5) 3))

;; Double Down
;; http://www.4clojure.com/problem/15
(= (* 2 2) 4)
(= ((partial * 2) 3) 6) ; best style
(= ((fn [x] (* 2 x)) 11) 22)
(= (#(* 2 %) 7) 14)

;; Hello World
;; http://www.4clojure.com/problem/16
(= ((fn [s] (str "Hello, " s "!")) "Dave") "Hello, Dave!")
(= (#(str "Hello, " % "!") "Jenn") "Hello, Jenn!")
(= (format "Hello, %s!" "Rhea") "Hello, Rhea!") ; best style

;; Sequences: map
;; http://www.4clojure.com/problem/17
(= '(6 7 8) (map #(+ % 5) '(1 2 3)))

;; Sequences: filter
;; http://www.4clojure.com/problem/18
(= '(6 7) (filter #(> % 5) '(3 4 5 6 7)))

;; Last Element
;; http://www.4clojure.com/problem/19
(= ((comp first reverse) [1 2 3 4 5]) 5)
(= (#(nth % (dec (count %))) '(5 4 3)) 3)
(= (#(first (into '() %)) ["b" "c" "d"]) "d")
;; from source of last function
((fn [s]
   (if (next s)
     (recur (next s))
     (first s))) [1 2 3 4 5])
;; similar to source
;; #(if (empty? (rest %)) (first %) (recur (rest %)))

;; Penultimate Element
;; http://www.4clojure.com/problem/20
(= ((comp second reverse) (list 1 2 3 4 5)) 4)
(= (#(nth % (- (count %) 2)) ["a" "b" "c"]) "b")
(= (#(if (empty? (rest (rest %))) (first %) (recur (rest %))) [[1 2] [3 4]]) [1 2])
;; #(first (rest (reverse %)))

;; Nth Element
;; http://www.4clojure.com/problem/21
(= (#(loop [coll %1
            index %2]
       (if (= index 0)
         (first coll)
         (recur (rest coll) (dec index))))
    '(4 5 6 7) 2) 6)
(= (#(first (drop %2 %1)) [:a :b :c] 0) :a)
(= (#((vec %) %2) [1 2 3 4] 1) 2)

;; Count a Sequence
;; http://www.4clojure.com/problem/22
(= ((fn count*
      ([coll] (count* coll 0))
      ([coll x]
       (cond
         (empty? coll) x
         :else (recur (rest coll) (inc x))))) '(1 2 3 3 1) 5))
(= ((fn my-count [items]
      (if (empty? items)
        0
        (inc (my-count (rest items))))) "Hello World") 11)
(= (#(apply + (map (fn [_] 1) %)) [[1 2] [3 4] [5 6]]) 3)
(= (#(reduce + (map (fn [x] 1) %)) '(13)) 1)
(= (#(.size (vec %)) '(:a :b :c)) 3)

;; Reverse a Sequence
;; http://www.4clojure.com/problem/23
(= (#(loop [coll %
            v []]
       (if (empty? coll)
         v
         (recur (rest coll) (cons (first coll) v)))) [1 2 3 4 5]) [5 4 3 2 1])
(= (reduce conj () (sorted-set 5 7 2 7)) '(7 5 2))
(= (into '() [[1 2] [3 4] [5 6]]) [[5 6] [3 4] [1 2]])

;; Sum It All Up
;; http://www.4clojure.com/problem/24
(= (reduce + [1 2 3]) 6)

;; Find the odd numbers
;; http://www.4clojure.com/problem/25
(= (filter odd? #{1 2 3 4 5}) '(1 3 5))

;; Fibonacci Sequence
;; http://www.4clojure.com/problem/26
(= ((fn [m] (map first
                 (take m (iterate (fn [[x y]]
                                    (vector y (+ x y))) [(bigint 1) (bigint 1)])))) 3) '(1 1 2))

;; Palindrome Detector
;; http://www.4clojure.com/problem/27
(false? (#(= (seq %) (reverse %)) '(1 2 3 4 5)))

;; Flatten a Sequence
;; http://www.4clojure.com/problem/28
; from the source
(= (#(filter (complement sequential?)
             (rest (tree-seq sequential? seq %))) '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
;; or
(fn c [s] (if (sequential? s) (mapcat c s) [s]))

;; Get the Caps
;; http://www.4clojure.com/problem/29
(= (#(apply str (re-seq #"[A-Z]+" %)) "HeLlO, WoRlD!") "HLOWRD")

;; Compress a Sequence
;; http://www.4clojure.com/problem/30
(= (apply str (#(->> % (partition-by identity) (map first)) "Leeeeeerrroyyy")) "Leroy")
;; (fn [x] (reduce #(if (not= (last %) %2) (conj % %2) %) [] x) )

;; Pack a Sequence
;; http://www.4clojure.com/problem/31
(= (#(partition-by identity %) [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))

;; Duplicate a Sequence
;; http://www.4clojure.com/problem/32
(= (#(interleave % %) [1 2 3]) '(1 1 2 2 3 3))
;; mapcat #(list % %)

;; Replicate a Sequence
;; http://www.4clojure.com/problem/33
(= (#(apply concat (map (fn [x] (repeat %2 x)) %)) [1 2 3] 2) '(1 1 2 2 3 3))
; #(mapcat (partial repeat %2) %1)

;; Implement range
;; http://www.4clojure.com/problem/34
(= ((fn [x y] (loop [v []
                     x x]
                (cond
                  (< x y) (recur (conj v x) (inc x))
                  :else v))) 1 4) '(1 2 3))
(= (#(take (- %2 %1) (iterate inc %1)) -2 2) '(-2 -1 0 1))

;; Local bindings
;; http://www.4clojure.com/problem/35
(= 7 (let [x 5] (+ 2 x)))
(= 7 (let [x 3, y 10] (- y x)))
(= 7 (let [x 21] (let [y 3] (/ x y))))

;; Let it Be
;; http://www.4clojure.com/problem/36
(= 10 (let [x 7
            y 3
            z 1]
        (+ x y)))
(= 4 (let [x 7
           y 3
           z 1] (+ y z)))
(= 1 (let [x 7
           y 3
           z 1] z))
;; [x 7 y 3 z 1]
;; [[x y z] [7 3 1]]

;; Regular Expressions
;; http://www.4clojure.com/problem/37
(= "ABC" (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))

;; Maximum value
;; http://www.4clojure.com/problem/38
(= ((fn [& more] (reduce (fn [x y] (if (< x y) y x)) more) 1 8 3 4) 8))
(= (#(last (sort %&)) 30 20) 30)
(= (#(- (apply min (map - %&))) 45 67 11) 67)

;; Interleave Two Seqs
;; http://www.4clojure.com/problem/39
(= ((fn [& x] (flatten (apply map vector x))) [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
;; #(mapcat list %1 %2)
;; mapcat vector

;; Interpose a Seq
;; http://www.4clojure.com/problem/40
(= ((fn [x y] (butlast (interleave y (repeat (count y) x)))) 0 [1 2 3]) [1 0 2 0 3])
; #(rest (mapcat list (repeat %1) %2))

;; Drop Every Nth Item
;; http://www.4clojure.com/problem/41
(= ((fn [x y] (mapcat
               #(if (= (count %) y) (butlast %) %)
               (partition-all y x))) [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
; #(mapcat (partial take (dec %2)) (partition-all %2 %1))


;; Factorial Fun
;; http://www.4clojure.com/problem/42
(= (#(reduce * (range 1 (inc %))) 5) 120)

;; Intro to Iterate
;; http://www.4clojure.com/problem/45
(= '(1 4 7 10 13) (take 5 (iterate #(+ 3 %) 1)))

;; Contain Yourself
;; http://www.4clojure.com/problem/47
(contains? #{4 5 6} 4)

;; Intro to some
;; http://www.4clojure.com/problem/48
6

;; Split a sequence
;; http://www.4clojure.com/problem/49
(= (#(vector (take % %2) (take-last (- (count %2) %) %2)) 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
; (juxt take drop) -- so cool
; #(vector (take %1 %2) (drop %1 %2))

;; Advanced Destructuring
;; http://www.4clojure.com/problem/51
[1 2 3 4 5]

;; Intro to Destructuring
;; http://www.4clojure.com/problem/52
(= [2 4] (let [[a b c d e] [0 1 2 3 4]] [c e]))

;; Simple Recursion
;; http://www.4clojure.com/problem/57
(= '(5 4 3 2 1) ((fn foo [x]
                   (when (> x 0)
                     (conj (foo (dec x)) x))) 5))

;; Map Construction
;; http://www.4clojure.com/problem/61
(= (#(reduce conj (map (fn [x y] {x y}) %1 %2)) [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
; #(apply hash-map (interleave %1 %2))

;; Re-implement Iterate
;; http://www.4clojure.com/problem/62
;qiuxiafei
(= (take 5 ((fn it [f x]
              (lazy-seq (cons x (it f (f x))))) #(* 2 %) 1)) [1 2 4 8 16])

;; Group a Sequence
;; http://www.4clojure.com/problem/63
(= ((fn [f s]
      (reduce (fn [agg r]
                (update-in agg [(f r)] #(if (nil? %) (vector r) (conj % r)))) {} s)) #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})

;; Intro to Reduce
;; http://www.4clojure.com/problem/64
(= 15 (reduce + [1 2 3 4 5]))
(=  0 (reduce + []))
(=  6 (reduce + 1 [2 3]))

;; Greatest Common Divisor
;; http://www.4clojure.com/problem/66
(= ((fn [x y] (let [mi (min x y)
                    ma (max x y)]
                (apply max (mapcat #(if (and
                                         (= (rem x %) 0)
                                         (= (rem y %) 0))
                                      [%]
                                      [1])
                                   (reverse (range 1 (inc mi))))))) 2 4) 2)
;; Euclid's method
(fn gcd [a b]
  (cond
    (= a b) a
    (> a b) (recur (- a b) b)
    :else (recur a (- b a))))

;; Recurring Theme
;; http://www.4clojure.com/problem/68
(= [7 6 5 4 3]
   (loop [x 5
          result []]
     (if (> x 0)
       (recur (dec x) (conj result (+ 2 x)))
       result)))

;; Rearranging Code: ->
;; http://www.4clojure.com/problem/71
(= (last (sort (rest (reverse [2 5 4 1 3 6]))))
   (-> [2 5 4 1 3 6]
       (reverse)
       (rest)
       (sort)
       (last))
   5)

;; http://www.4clojure.com/problem/72
;; Rearranging Code: ->>
(= (reduce + (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
   (->> [2 5 4 1 3 6]
        (drop 2)
        (take 3)
        (map inc)
        (reduce +))
   11)
;; apply works too


;; Set Intersection
;; http://www.4clojure.com/problem/81
(= ((fn [x y] (set (filterv #(and (contains? x %) (contains? y %)) (into x y)))) #{0 1 2 3} #{2 3 4 5}) #{2 3})
; #(set (filter % %2))


;; A Half-Truth
;; http://www.4clojure.com/problem/83
(= false (#(->> (set %&)
                count
                (= 2)) false false))

;; Symmetric Difference
;; http://www.4clojure.com/problem/88
(= (#(clojure.set/difference (clojure.set/union % %2) (clojure.set/intersection % %2)) #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})

;; Cartesian Product
;; http://www.4clojure.com/problem/90
(= ((fn cartez [y x]
      (set (mapcat (fn [a]
                     (map #(vec [% a]) y)) x))) #{1 2 3} #{4 5})
   #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})
; #(set (for [x % y %2] [x y]))

;; To Tree, or not to Tree
;; http://www.4clojure.com/problem/95
(fn istree? [root]
  (or (nil? root)
      (and (sequential? root)
           (= 3 (count root))
           (every? istree? (rest root)))))

;; Beauty is Symmetry
;; http://www.4clojure.com/problem/96
; qiuxiafei
(fn symmetry [[root left right]]
  (let [mirror? (fn mirror? [a b]
                  (cond
                    (not= (sequential? a) (sequential? b)) false
                    (sequential? a) (let [[ra La Ra] a
                                          [rb Lb Rb] b]
                                      (and (= ra rb) (mirror? La Rb) (mirror? Lb Ra)))
                    :else (= a b)))])
  (mirror? left right))

;; Pascal's Triangle
;; http://www.4clojure.com/problem/97
(fn [s]
  (reduce (fn [agg p]
            (cond
              (< (count agg) 2) (into agg (vector p))
              :else (into (into (vector p) (map #(apply + %) (partition 2 1 agg))) (vector p))))
          [] (repeat s 1)))
(defn pascal [x]
  (mapv (fn [[x y]] (+ x y)) (partition 2 1 (into [0] (into x [0])))))
(defn solve [p]
  (map #(println (clojure.string/join " " %)) (take p (iterate pascal [1]))))
(solve 5)

;; Product Digits
;; http://www.4clojure.com/problem/99
(= ((fn [a b] (map #(Integer/parseInt (str %))  (str (* a b)))) 1 1) [1])
; #(map (comp read-string str) (str (* % %2)))

;; Least Common Multiple
;; http://www.4clojure.com/problem/100
;Sonia Hamilton
(fn lcm3
  ([x y
    (letfn [(gcd2 [a b]
              (cond
                (= b 0) a
                (> a b) (gcd2 b (mod a b))
                (> b a) (gcd2 a (mod b a))))]
      (/ (* x y) (gcd2 x y)))])
  ([x y & rest] (apply lcm3 (lcm3 x y) rest)))

;; Simple closures
;; http://www.4clojure.com/problem/107
(= 256 (((fn [y] (fn [x] (int (Math/pow x y)))) 2) 16)
   (((fn [y] (fn [x] (int (Math/pow x y)))) 8) 2))

;; Through the Looking Class
;; http://www.4clojure.com/problem/126
Class

;; A nil key
;; http://www.4clojure.com/problem/134
(true?  (#(and (contains? %2 %1) (nil? (%1 %2))) :a {:a nil :b 2}))
(false? (#(nil? (%1 %2 :not-found)) :b {:a nil :b 2}))
(false? (#((comp boolean (set %2)) [%1 nil]) :c {:a nil :b 2})) ; interesting :)

;; For the win
;; http://www.4clojure.com/problem/145
(= '(1 5 9 13 17 21 25 29 33 37) (for [x (range 40)
                                       :when (= 1 (rem x 4))]
                                   x))
(= (range 1 40 4) (for [x (iterate #(+ 4 %) 0)
                        :let [z (inc x)]
                        :while (< z 40)]
                    z))
(= '(1 5 9 13 17 21 25 29 33 37) (for [[x y] (partition 2 (range 20))]
                                   (+ x y)))

;; Map Defaults
;; http://www.4clojure.com/problem/156
(= ((fn [x v]
      (reduce #(conj %1 {%2 x}) {} v)) 0 [:a :b :c]) {:a 0 :b 0 :c 0})
(= (#(zipmap %2 (repeat %)) "x" [1 2 3]) {1 "x" 2 "x" 3 "x"})
(= (#(apply hash-map (interleave %2 (repeat %1))) [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})
;; (into {} (#(for [v %2] (hash-map v %1)) 0 [:a :b :c]))

;; Logical falsity and truth
;; http://www.4clojure.com/problem/162
(= 1 (if-not false 1 0))
(= 1 (if-not nil 1 0))
(= 1 (if true 1 0))
(= 1 (if [] 1 0))
(= 1 (if [0] 1 0))
(= 1 (if 0 1 0))
(= 1 (if 1 1 0))

;; Subset and Superset
;; http://www.4clojure.com/problem/161
(clojure.set/superset? #{1 2} #{2})
(clojure.set/subset? #{1} #{1 2})
(clojure.set/superset? #{1 2} #{1 2})
(clojure.set/subset? #{1 2} #{1 2})

;; Comparisons
;; http://www.4clojure.com/problem/166
(= :gt ((fn [f x y] (cond
                      (f x y) :lt
                      (f y x) :gt
                      :else :eq)) < 5 1))

;;****************


(defn binary-tree? [[root left right & more]]
  (cond
    (sequential? root) false
    (some nil? [root left right]) false
    (not (nil? more)) false
    (every? #(or (not (sequential? %))
                 (and (= 3 (count %))
                      (binary-tree? %)))
            [left right]) true
    :else false))

(binary-tree? [1 2 [1 2 3]])
