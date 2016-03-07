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
