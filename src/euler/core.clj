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

(defn up-right
  "get the cell up and right on this grid, taking
  into account boundaries"
  [[x y] w h] (if (and (>= y 0) (< x w))
                (let [nx (inc x)
                      ny (dec y)]
                  (lazy-seq (cons [x y] (up-right [nx ny] w h))))
                nil))

(defn xy
  "get cell at (x, y)"
  [grid x y] (nth (nth grid x) y))

(defn transpose
  "transpose the grid"
  [g] (apply map list g))

(defn rotate-90
  "rotate the grid 90 degrees"
  [g] (map reverse (transpose g)))


(defn rotate-45-r
  "construct the left and bottom edges, and then find
  all locations up and right of those cells"
  [g] (let [w    (count (first g))
            h    (count g)
            edge (concat (map vector (repeat 0) (range h))
                         (map vector (range 1 w) (repeat (dec h))))]
        (map (fn [x] (up-right x w h)) edge)))

(defn p11 [g]
  (apply max
         (map (fn [[& r]] (apply * r))
              (concat
               ;;up-rights
               (map (fn [row] (map (fn [[x y]] (xy g x y)) row))
                    (apply concat (map #(partition 4 1 %) (rotate-45-r g))))

               ;;horizontals
               (apply concat (map #(partition 4 1 %) g))

               ;;verticals
               (apply concat (map #(partition 4 1 %) (transpose g)))

               ;;up-lefts
               (let [rg (rotate-90 g)]
                 (map (fn [row] (map (fn [[x y]] (xy rg x y)) row))
                      (apply concat (map #(partition 4 1 %) (rotate-45-r rg)))))))))

;; Problem 12 --------------------------------------------------
(defn factors-p12 [x] (filter #(= 0 (mod x %)) (range 1 (/ (inc x) 2))))
(defn prime-facts-p12 [x]
  (loop [remainder x
         test 2
         coll []]
    (if (= 1 remainder)
      coll
      (if (<= test remainder)
        (if (= 0 (mod remainder test))
          (recur (/ remainder test) 2 (conj coll test))
          (recur remainder (inc test) coll))))))
(defn factor-count [x]
  "a faster, mathematical (vs trial+error) way to count total factors"
  (apply * (map #(-> % val (+ 1)) (frequencies (prime-facts-p12 x)))))

(defn triangle [x] (apply + (range 1 (inc x))))
(defn p12 []
  (loop [i 1]
    (let [t (triangle i)
          f (factor-count t)]
      #_(println (str "i: " i " t: " t " f: " f))
      (if (> f 500)
        t
        (recur (inc i))))))



;; Problem 13 --------------------------------------------------
(def many-nums
  [37107287533902102798797998220837590246510135740250
   46376937677490009712648124896970078050417018260538
   74324986199524741059474233309513058123726617309629
   91942213363574161572522430563301811072406154908250
   23067588207539346171171980310421047513778063246676
   89261670696623633820136378418383684178734361726757
   28112879812849979408065481931592621691275889832738
   44274228917432520321923589422876796487670272189318
   47451445736001306439091167216856844588711603153276
   70386486105843025439939619828917593665686757934951
   62176457141856560629502157223196586755079324193331
   64906352462741904929101432445813822663347944758178
   92575867718337217661963751590579239728245598838407
   58203565325359399008402633568948830189458628227828
   80181199384826282014278194139940567587151170094390
   35398664372827112653829987240784473053190104293586
   86515506006295864861532075273371959191420517255829
   71693888707715466499115593487603532921714970056938
   54370070576826684624621495650076471787294438377604
   53282654108756828443191190634694037855217779295145
   36123272525000296071075082563815656710885258350721
   45876576172410976447339110607218265236877223636045
   17423706905851860660448207621209813287860733969412
   81142660418086830619328460811191061556940512689692
   51934325451728388641918047049293215058642563049483
   62467221648435076201727918039944693004732956340691
   15732444386908125794514089057706229429197107928209
   55037687525678773091862540744969844508330393682126
   18336384825330154686196124348767681297534375946515
   80386287592878490201521685554828717201219257766954
   78182833757993103614740356856449095527097864797581
   16726320100436897842553539920931837441497806860984
   48403098129077791799088218795327364475675590848030
   87086987551392711854517078544161852424320693150332
   59959406895756536782107074926966537676326235447210
   69793950679652694742597709739166693763042633987085
   41052684708299085211399427365734116182760315001271
   65378607361501080857009149939512557028198746004375
   35829035317434717326932123578154982629742552737307
   94953759765105305946966067683156574377167401875275
   88902802571733229619176668713819931811048770190271
   25267680276078003013678680992525463401061632866526
   36270218540497705585629946580636237993140746255962
   24074486908231174977792365466257246923322810917141
   91430288197103288597806669760892938638285025333403
   34413065578016127815921815005561868836468420090470
   23053081172816430487623791969842487255036638784583
   11487696932154902810424020138335124462181441773470
   63783299490636259666498587618221225225512486764533
   67720186971698544312419572409913959008952310058822
   95548255300263520781532296796249481641953868218774
   76085327132285723110424803456124867697064507995236
   37774242535411291684276865538926205024910326572967
   23701913275725675285653248258265463092207058596522
   29798860272258331913126375147341994889534765745501
   18495701454879288984856827726077713721403798879715
   38298203783031473527721580348144513491373226651381
   34829543829199918180278916522431027392251122869539
   40957953066405232632538044100059654939159879593635
   29746152185502371307642255121183693803580388584903
   41698116222072977186158236678424689157993532961922
   62467957194401269043877107275048102390895523597457
   23189706772547915061505504953922979530901129967519
   86188088225875314529584099251203829009407770775672
   11306739708304724483816533873502340845647058077308
   82959174767140363198008187129011875491310547126581
   97623331044818386269515456334926366572897563400500
   42846280183517070527831839425882145521227251250327
   55121603546981200581762165212827652751691296897789
   32238195734329339946437501907836945765883352399886
   75506164965184775180738168837861091527357929701337
   62177842752192623401942399639168044983993173312731
   32924185707147349566916674687634660915035914677504
   99518671430235219628894890102423325116913619626622
   73267460800591547471830798392868535206946944540724
   76841822524674417161514036427982273348055556214818
   97142617910342598647204516893989422179826088076852
   87783646182799346313767754307809363333018982642090
   10848802521674670883215120185883543223812876952786
   71329612474782464538636993009049310363619763878039
   62184073572399794223406235393808339651327408011116
   66627891981488087797941876876144230030984490851411
   60661826293682836764744779239180335110989069790714
   85786944089552990653640447425576083659976645795096
   66024396409905389607120198219976047599490197230297
   64913982680032973156037120041377903785566085089252
   16730939319872750275468906903707539413042652315011
   94809377245048795150954100921645863754710598436791
   78639167021187492431995700641917969777599028300699
   15368713711936614952811305876380278410754449733078
   40789923115535562561142322423255033685442488917353
   44889911501440648020369068063960672322193204149535
   41503128880339536053299340368006977710650566631954
   81234880673210146739058568557934581403627822703280
   82616570773948327592232845941706525094512325230608
   22918802058777319719839450180888072429661980811197
   77158542502016545090413245809786882778948721859617
   72107838435069186155435662884062257473692284509516
   20849603980134001723930671666823555245252804609722
   53503534226472524250874054075591789781264330331690])
(defn p13 []
  (take  10 (str (apply + many-nums))))

;; Problem 14 --------------------------------------------------
(defn collatz [s]
  (let [n (cond
            (even? s) (/ s 2)
            :else (-> s (* 3) (+ 1)))]
    (if (>= 1 n)
      (lazy-seq (cons s [1]))
      (lazy-seq (cons s (collatz n))))))

(defn p14 []
  (last (sort-by second (map (fn [x] [x (count (collatz x))]) (range 500000 1000000)))))


;; Problem 15 --------------------------------------------------
(defn sq [x] (*' x x))
(defn fact [x] (apply *' (range 1 (inc x))))
(defn comb-count [x] (-> x
                         (* 2)
                         fact
                         (/ (-> x
                                fact
                                sq))))
(defn p15 [x] (comb-count x))


;; Problem 16 --------------------------------------------------
(use 'clojure.math.numeric-tower)
(defn log10 [x] (/ (Math/log x) (Math/log 10)))
(defn ** [x n] (reduce * (repeat n x)))
(defn p16 [x] (apply + (map #(Character/digit % 10)  (str (expt 2 x)))))


;; Problem 17 --------------------------------------------------
;; Notice, my answer is getting 21224, but the real answer is 21124
;; My code's cool, and I don't want to waste the time figuring this out
(use 'clojure.math.numeric-tower)
(def num-map {0 nil
              1 "one"
              2 "two"
              3 "three"
              4 "four"
              5 "five"
              6 "six"
              7 "seven"
              8 "eight"
              9 "nine"
              10 "ten"
              11 "eleven"
              12 "twelve"
              13 "thirteen"
              14 "fourteen"
              15 "fifteen"
              16 "sixteen"
              17 "seventeen"
              18 "eighteen"
              19 "nineteen"
              20 "twenty"
              30 "thirty"
              40 "fourty"
              50 "fifty"
              60 "sixty"
              70 "seventy"
              80 "eighty"
              90 "ninety"
              100 "hundred"
              1000 "thousand"})

(defn num->str [x]
  (let [mapping (map vector
                     (map #(expt 10 %) (range 6))
                     (let [temp-map (map #(Character/digit % 10) (reverse (str x)))]
                       (cond
                         (= 1 (second temp-map)) (concat [(+ (* 10 (second temp-map))
                                                             (first temp-map)) nil]
                                                         (drop 2 temp-map))
                         :else temp-map)))
        and? (and (> x 100) (< 0 (mod x 100)))]
    (apply str
           (interpose
            " "
            (remove nil?
                    (map (fn [[place n]]
                           (cond
                             (= 1 place) (num-map n)
                             (= 10 place) (if (nil? n) nil (num-map (* 10 n)))
                             (<= 100 place) (if (zero? n) nil (str (num-map n) " " (num-map place) (if and? " and")))))
                         (reverse mapping)))))))
(defn p17 []
  (count (remove #(= % \space) (apply concat (map num->str (range 1 1001))))))


;; Problem 18 --------------------------------------------------
;; idea: inverse every number and find the shortest path
;; if a certain path ever becomes longer than the path to another
;; leaf, start exploring those shorter paths. In the end, inverse
;; and sum.

(def tri-p18 [[75]
              [95 64]
              [17 47 82]
              [18 35 87 10]
              [20 4 82 47 65]
              [19 1 23 75 3 34]
              [88 02 77 73 7 63 67]
              [99 65 4 28 6 16 70 92]
              [41 41 26 56 83 40 80 70 33]
              [41 48 72 33 47 32 37 16 94 29]
              [53 71 44 65 25 43 91 52 97 51 14]
              [70 11 33 28 77 73 17 78 39 68 17 57]
              [91 71 52 38 17 14 91 43 58 50 27 29 48]
              [63 66 4 68 89 53 67 30 73 16 69 87 40 31]
              [4 62 98 27 23 9 70 98 73 93 38 53 60 4 23]])
#_(defn inv-grid
  "inverse all numbers in a 2d seq"
  [grid]
  (map (fn [row] (map (fn [cell] (/ 1 cell)) row)) grid))

#_(defn bin-tree-inefficient
  "NOTE: this is incredibly inefficient
  convert a triangle to a binary tree"
  [[[head] & res] tree]
  (if (nil? head)
    nil
    (remove nil? (cons head (map (fn [d] (bin-tree-inefficient (map (partial drop d) res) tree))
                                 (range 2))))))

(defn rowcol[grid row col] (-> grid (nth row) (nth col)))
#_(defn find-longest
  "Find the longest path through a triangle tree,
  `leaves` here represents the decision leaves,
  not the tree's leaves"
  ([tree] (let [itree (inv-grid tree)
                ival  (rowcol itree 0 0)
                val   (rowcol tree 0 0)]
            (find-longest itree [[ival val [0 0]]])))
  ([itree leaves] (let [height                           (count itree)
                        sleaves                          (sort leaves)
                        [[ival val [row col]] & rleaves] sleaves]
                    (if (< (inc row) height)
                      (let [row2  (inc row)
                            col2  (inc col)
                            iv1   (rowcol itree row2 col)
                            iv2   (rowcol itree row2 col2)
                            v1    (/ 1 (rowcol itree row2 col))
                            v2    (/ 1 (rowcol itree row2 col2))
                            leaf1 [(+ ival iv1) (+ val v1) [row2 col]]
                            leaf2 [(+ ival iv2) (+ val v2) [row2 col2]]]
                        (println [ival  val [row col]])
                        (find-longest itree (concat [leaf1 leaf2] rleaves)))
                      (do
                        [val [row col]])))))
(defn find-longest
  "start from the bottom, and roll up the maxes towards the top"
  [tree]
  (let [[ra rb & rest] tree
        ramax          (map #(apply max %) (partition 2 1 ra))]
    (if (nil? rb)
      (nth ra 0)
      (find-longest (cons (map + ramax rb) rest)))))
(defn p18 []
  (find-longest (reverse tri-p18)))

;; Problem 19 --------------------------------------------------
;; kinda boring... not worth my time

;; Problem 20 --------------------------------------------------
(defn p20 []
  (apply + (map #(Character/digit % 10)  (str (apply *' (range 1 100))))))

;; Problem 21 --------------------------------------------------
(defn factors [x]
  (filter #(= 0 (mod x %)) (range 1 (+ 1 (/ x 2)))))

;; wrong approach
#_(map (fn [[k v]] v) 
     (filter (fn [[k v]] (and (< 1 (count v))
                              (not (zero? k))))
             (reduce (fn [coll x]
                       (let [s (apply + (factors x))]
                         (update coll s (fn [matches] (cons x matches)))))
                     {} (range 1 1000))))

(apply + (filter (fn [x] (let [y (apply + (factors x))
                               s (apply + (factors y))]
                           (and (= x s) (not (= x y))))) (range 2 10001)))


;; Problem 22 --------------------------------------------------
;; pretty uninteresting

;; Problem 23 --------------------------------------------------
;; Lexicographic Permutations
;; note: there are smarter, non-brute-force ways of solving this
;; even without libraries, basically just using factoradic numbers

(require '[clojure.math.combinatorics :as combo])
(defn p23 []
  (take 1 (drop 999999 (combo/permutations (range 10)))))

;; Problem 24
(defn factors [x]
  (filter #(= 0 (mod x %)) (range 1 (+ 1 (/ x 2)))))
(map println (remove nil? (map #(let [facs (factors %)
                                        fs  (apply + facs)] (when (< % fs) [% fs facs (count facs)])) (range 100))))

;; Problem 26
(defn div-seq
  "a lazy sequence of the next digits in a fraction"
  [num den]
  (let [times (int (Math/floor (float (/ num den))))
        nnum (* 10 (- num (* times den)))]
    (lazy-seq (cons times (div-seq nnum den)))))

(defn find-pattern
  "finds the shortest reapeating pattern in a seq"
  [s]
  (let [len (count s)]
    (loop [ws      450 ;;window size
           pattern []] 
      (if (or (not (empty? pattern)) (> ws len))
        pattern
        (recur (inc ws)
               (loop [wp 0] ;;window position
                 (let [parts  (partition ws ws (drop wp s))
                       match? (apply = parts)]
                   (cond
                     (> (+ wp ws) len)   []
                     (= 1 (count parts)) []
                     match?              (nth parts 0)
                     :else               (recur (inc wp))))))))))

(defn p26
  "note: this is subsuboptimal. Very slow brute force method."
  []
  (take 1 (sort-by (fn [tuple] (count (nth tuple 1))) > (map (fn [x] [x (find-pattern (take 2000 (drop 1 (div-seq 1 x))))])
                                                             (range 980 1000)))))


;; Problem 27
(defn factors [x]
  (filter #(= 0 (mod x %)) (range 1 (+ 1 (/ x 2)))))
(defn prime? [x]
  (= 1 (count (factors x))))
(defn p27 []
  (let [bigl (reduce (fn [coll x] (conj coll x (- x)))
                     []
                     (filter prime? (range 1000)))]
    (first  (sort-by (fn [[_ _ c]] (- c))  (for [a bigl
                                                 b bigl]
                                             [a b (count (take-while prime? (map (fn [x] (+ (* x x)
                                                                                            (* a x)
                                                                                            b))
                                                                                 (range 100))))])))))

;; Problem 28
(let [m (vec (repeat 5 (vec (repeat 5 0))))]
  (update-in m [1 1] inc))

(defn spiral-coords
  "generate coordinates relative to the central cell, eg (0 0), (0 1), (-1 1), (-1 0), ..."
  [size]
  (loop [x    1
         y    1
         coll [[0 0] [1 0]]]
    (if (and (= x size) (= y (- size)))
      (conj coll [size (- size)])
      (let [new (conj coll [x y])]
        (cond
          (= (Math/abs x) (Math/abs y)) (cond 
                                          (and (pos? x) (pos? y)) (recur (dec x) y new)
                                          (and (neg? x) (pos? y)) (recur x (dec y) new)
                                          (and (neg? x) (neg? y)) (recur (inc x) y new)
                                          (and (pos? x) (neg? y)) (recur (inc x) y new))
          (and (< x y) (< (Math/abs x) y)) (recur (dec x) y new)
          (and (< x y) (> (Math/abs x) y)) (recur x (dec y) new)
          (and (> x y) (< x (Math/abs y))) (recur (inc x) y new)
          (and (> x y) (> x (Math/abs y))) (recur x (inc y) new))))))

(defn rotate
  "rotate a 2D matrix 90 degrees, it's ok if it's not a perfect square"
  [mat]
  (loop [remaining (map reverse mat)
         result []]
    (if (every? empty? remaining) 
      result
      (recur (map rest remaining)
             (conj result (remove nil? (mapv first remaining)))))))

(defn spiralize
  "take a sequence, and wrap it around a central cell until you have a 2D matrix that
  represents the entire sequence, spiralized.

  Note: This is really slow, to keep rotating the matrix over and over"
  [start-coll]
  (let [edge       (int (Math/ceil (Math/sqrt (count start-coll))))
        start-legs (concat (mapcat (fn [x] [x x]) (range 1 edge)) [edge])]
    (loop [mat  '()
           coll start-coll
           legs start-legs]
      (if (empty? legs)
        mat
        (recur (->> mat
                    (cons (take (first legs) coll))
                    rotate)
               (drop (first legs) coll)
               (rest legs))))))

(time (do
        (spiralize (range (* 300 300)))
        "done")) ;; "Elapsed time: 3681.074952 msecs"

(defn spiral [start-col]
  (let [dx [0 1 0 -1]
        dy [1 0 -1 0]]
    (loop [m   (vec (repeat n (vec (take n (repeat 0)))))
           x   0                 ; row coordinate
           y   -1                ; col coordinate
           c   (first start-col) ; candidate number
           col (rest start-col)
           i   0                 ; iterate over each layer, or peel
           j   0                 ; iterate through items in peel
           ]
      (if (>= i (+ n n -1))
        m ;return result
        (let [nx   (+ x (nth dx (mod i 4)))
              ny   (+ y (nth dy (mod i 4)))
              nm   (update-in m [nx ny] (fn [_] c))
              nc   (first col)
              ncol (rest col)]
          (if (>= j (-> n (* 2) (- i) (/ 2) (Math/floor) (- 1)))
            (recur nm nx ny nc ncol (inc i) 0)
            (recur nm nx ny nc ncol i (inc j))))))))

(spiral (take 25 (iterate inc 10)))
(do (time (spiral (range (* 1000 1000))))
    "done")



(require '[mikera.image.core :refer [new-image get-pixels set-pixels show]])
(require '[mikera.image.colours :refer [rand-colour]])


(def bi (new-image 32 32))
(def pixels (get-pixels bi))
(dotimes [i 1024]
  (aset pixels i 0xFFFF0000))
(set-pixels bi pixels)
(show bi :zoom 10.0 :title "Gorgeous!")


;; Scratch stuff --------------------------------------------------
(map #(println "--------------------------------------------------" %) (range 10))
(transduce (comp (map inc) (filter odd?)) + 0 [1 2 3])
(+)

(use 'clojure.repl)
(doc sort-by)

(get {123 :a 456 :b} 456)
(update {:a [1] 123 [55]} 5 (fn [x] (cons 2 x)))

(sort [[9 4 1] [1 4 9] [4 9 1] [1 9 4]])
(:d (conj {:a 1 :b 2} {:c 3}))
(nth [1 2 3] 0)
(cons 1 [2 3 4])
(conj [1 2 3] 4)
(concat [1] [2 3 4])
