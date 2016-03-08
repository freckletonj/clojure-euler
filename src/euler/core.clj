(ns euler.core)


;; Problem 1 --------------------------------------------------
(defn p1 []
  (apply + (filter #(or (= 0 (mod % 3))
                        (= 0 (mod % 5)))
                   (range 1000))))

;; Problem 2 --------------------------------------------------
(defn fibs
  ([] (fibs 0 1))
  ([a b] (let [c (+ a b)] (lazy-seq (cons c (fibs b c))))))

(defn p2 []
  (apply +  (filter #(= 0 (mod % 2)) (take-while #(< % 4000000) (fibs)))))


;; Problem 3 --------------------------------------------------
(defn factors [x]
  (filter #(= 0 (mod x %)) (range 2 (-> x
                                        Math/sqrt
                                        Math/ceil
                                        int
                                        (+ 1)))))
(defn p3 []
  (last (filter #(= 0 (count (factors %)))
                (factors 600851475143))))


;; Problem 4 --------------------------------------------------
(defn palindrome? [x] (= (reverse (str x)) (seq (str x))))

(defn p4 []
  (apply max
         (filter palindrome?
                 (for
                     [a (range 100 1000)
                      b (range 100 1000)]
                   (* a b)))))

;; Problem 5 --------------------------------------------------

(defn prime-facts [x]
  (loop [remainder x
         test 2
         coll []]
    (if (= 1 remainder)
      coll
      (if (<= test remainder)
        (if (= 0 (mod remainder test))
          (recur (/ remainder test) 2 (conj coll test))
          (recur remainder (inc test) coll))))))

(defn p5 [s]
  (apply * (loop [coll    []
                  [f & r] (range 2 (inc s))]
             (if (= 0 (count r))
               (conj coll f)
               (recur (conj coll f) (map (fn [x] (if (= 0 (mod x f)) (/ x f) x)) r))))))

;; Problem 6 --------------------------------------------------
(- (apply + (map #(* % %) (range 101)))
   (let [x (apply + (range 101))] (* x x)))


;; Problem 7 --------------------------------------------------
(defn prime? [n]
  (cond (<= n 1) false
        (= n 2) true
        :else (loop [f 2]
                (cond (zero? (rem n f)) false
                      (> f (Math/sqrt n)) true
                      :else (recur (inc f))))))
(defn prime-seq
  "list of primes bigger than x"
  [x]
  (let [p (loop [n x]
            (if (prime? n)
              n
              (recur (inc n))))]
    (lazy-seq (cons p (prime-seq (inc p)) ))))

(last (take 10002 (prime-seq 1)))
(take 2 (prime-seq 104729))

;; Problem 8 --------------------------------------------------
(def bign "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")


(defn p8 []
  (last (sort-by first
                 (map
                  (fn [x] [(apply * (map #(Integer. (str %)) x)) x])
                  (partition 13 1 bign)))))

;; Problem 9 --------------------------------------------------
(defn pyth-trip? [a b c] (= (+ (* a a) (* b b)) (* c c)))

(defn p9 []
  (remove nil? (for [a (range 1 1000)
                     b (range 1 1000)
                     :let [c (- 1000 a b)]]
                 (if (pyth-trip? a b c)
                   (* a b c)))))

;; Problem 10 --------------------------------------------------
(defn prime? [n]
  (cond (<= n 1) false
        (= n 2) true
        :else (loop [f 2]
                (cond (zero? (rem n f)) false
                      (> f (Math/sqrt n)) true
                      :else (recur (inc f))))))
(defn prime-seq
  "list of primes bigger than x"
  [x]
  (let [p (loop [n x]
            (if (prime? n)
              n
              (recur (inc n))))]
    (lazy-seq (cons p (prime-seq (inc p)) ))))

(defn p10 []
  (apply + (take-while #(< % 2000000) (prime-seq 1))))


;; Problem 11 --------------------------------------------------
(def p10grid [[8 2 22 97 38 15 0 40 0 75 4 5 7 78 52 12 50 77 91 8]
              [49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 4 56 62 0]
              [81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 3 49 13 36 65]
              [52 70 95 23 4 60 11 42 69 24 68 56 1 32 56 71 37 2 36 91]
              [22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80]
              [24 47 32 60 99 3 45 2 44 75 33 53 78 36 84 20 35 17 12 50]
              [32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70]
              [67 26 20 68 2 62 12 20 95 63 94 39 63 8 40 91 66 49 94 21]
              [24 55 58 5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72]
              [21 36 23 9 75 0 76 44 20 45 35 14 0 61 33 97 34 31 33 95]
              [78 17 53 28 22 75 31 67 15 94 3 80 4 62 16 14 9 53 56 92]
              [16 39 5 42 96 35 31 47 55 58 88 24 0 17 54 24 36 29 85 57]
              [86 56 0 48 35 71 89 7 5 44 44 37 44 60 21 58 51 54 17 58]
              [19 80 81 68 5 94 47 69 28 73 92 13 86 52 17 77 4 89 55 40]
              [4 52 8 83 97 35 99 16 7 97 57 32 16 26 26 79 33 27 98 66]
              [88 36 68 87 57 62 20 72 3 46 33 67 46 55 12 32 63 93 53 69]
              [4 42 16 73 38 25 39 11 24 94 72 18 8 46 29 32 40 62 76 36]
              [20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 4 36 16]
              [20 73 35 29 78 31 90 1 74 31 49 71 48 86 81 16 23 57 5 54]
              [1 70 54 71 83 51 54 69 16 92 33 48 61 43 52 1 89 19 67 48]])

(def simple-grid (partition 5 (range 25)))

(defn up-right [[x y] w h] (if (and (>= y 0) (< x w))
                             (let [nx (inc x)
                                   ny (dec y)]
                               (lazy-seq (cons [x y] (up-right [nx ny] w h))))
                             nil))
;;(doall (up-right [1 2] 3 3))

(defn xy "get cell at (x, y)"
  [grid x y] (nth (nth grid x) y))

(xy simple-grid 1 2)

(defn rotate-45-r
  "construct the left and bottom edges, and then find
  all locations up and right of those cells"
  [g] (let [w    (count (first g))
            h    (count g)
            edge (concat (map vector (repeat 0) (range h))
                         (map vector (range 1 w) (repeat (dec h))))]
        (map (fn [x] (up-right x w h)) edge)))

;;up-rights
(map (fn [row] (map (fn [[x y]] (xy p10grid x y)) row))
     (apply concat (map #(partition 4 1 %) (rotate-45-r p10grid))))

;;horizontals
(map #(partition 4 1 %) p10grid)


(map vector (range 10) (repeat 0))
