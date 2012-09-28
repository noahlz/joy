(ns joy.performance)

(set! *warn-on-reflection* true)

;; Note: in Clojure 1.4 this simply doesn't work without ^floats due to aget and aset
;; changed to only work with reference arrays.
;; Also had to initialize ret to 0.0 to remove the following warning:
;; "recur arg for primitive local: ret is not matching primitive, had: double, needed: long"
;; Also, had to decorate the function with ^Double instead of ^Float, or else ClassCastException
(defn ^Double asum-sq [^floats xs]
  (let [^floats dbl (amap xs i ret
                       (* (aget xs i)
                       (aget xs i)))]
    (areduce dbl i ret 0.0
      (+ ret (aget dbl i)))))
;; Warnings before type hints added:
;; Reflection warning, joy/performance.clj:6 - call to aclone can't be resolved.
;; Reflection warning, joy/performance.clj:6 - call to alength can't be resolved.
;; Reflection warning, joy/performance.clj:7 - call to aget can't be resolved.
;; Reflection warning, joy/performance.clj:8 - call to aget can't be resolved.
;; Reflection warning, joy/performance.clj:6 - call to aset can't be resolved.
;; Reflection warning, joy/performance.clj:9 - call to alength can't be resolved.
;; Reflection warning, joy/performance.clj:10 - call to aget can't be resolved.
;; performance.clj:9 recur arg for primitive local: ret is not matching primitive, had: Object, needed: long
;; Auto-boxing loop arg: ret
;; Reflection warning, joy/performance.clj:9 - call to alength can't be resolved.
;; Reflection warning, joy/performance.clj:10 - call to aget can't be resolved.

(defn zencat1 [x y]
  (loop [src y, ret x]
    (if (seq src)
      (recur (next src) (conj ret (first src)))
      ret)))

(defn zencat2 [x y]
  (loop [src y, ret (transient x)]
    (if (seq src)
      (recur (next src) (conj! ret (first src)))
      (persistent! ret))))

(def big-vector (vec (range 1e6)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; De-Chunkifying Lazy Seqs

(def gimmie #(do (print \.) %))

(defn seq1 [s]
  (lazy-seq
    (when-let [[x] (seq s)]
      (cons x (seq1 (rest s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Memoization

(def gcd (memoize
           (fn [x y]
             (cond
               (> x y) (recur (- x y) y)
               (< x y) (recur x (- y x))
               :else x))))

;; See: http://kotka.de/blog/2010/03/memoize_done_right.html

(defprotocol CacheProtocol
  (lookup [cache e])
  (has?   [cache e])
  (hit    [cache e])
  (miss   [cache e ret]))

(deftype BasicCache [cache]
  CacheProtocol
  (lookup [_ item]
    (get cache item))
  (has? [_ item]
    (contains? cache item))
  (hit [this item] this)
  (miss [_ item result]
    (BasicCache. (assoc cache item result))))

;; “pass an element through the cache and return its value”
;; The use of an explicit delay helps to ensure that the value
;; is calculated only on first retrieval.
(defn through [cache f item]
  (if (has? cache item)
    (hit cache item)
    (miss cache item (delay (apply f item)))))

(deftype PluggableMemoization [f cache]
  CacheProtocol
  (has? [_ item] (has? cache item))
  (hit  [this item] this)
  (miss [_ item result]
    (PluggableMemoization. f (miss cache item result)))
  (lookup [_ item]
    (lookup cache item)))

(defn memoization-impl [cache-impl]
  (let [cache (atom cache-impl)]
    (with-meta
      (fn [& args]
        (let [cs (swap! cache through (.f cache-impl) args)]
          @(lookup cs args)))
    {:cache cache})))

(def slowly (fn [x] (Thread/sleep 3000) x))

(def sometimes-slowly (memoization-impl
                        (PluggableMemoization.
                          slowly
                          (BasicCache. {}))))

;; You can now fulfill your personalized memoization needs by implementing pointed
;; realizations of CacheProtocol, plugging them into instances of PluggableMemoization,
;; and applying them as needed via function redefinition, higher-order functions, or
;; dynamic binding. Countless caching strategies can be used to better support your needs,
;; each displaying different characteristics, or if needed your problem may call for some­
;; thing wholly new.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 12.5 Understanding coercion

(defn occur-count [words]
  (let [res (atom {})]
    (doseq [w words] (swap! res assoc w (+ 1 (@res w 0))))
    @res))

(defn roll [n d]
  (reduce + (take n (repeatedly #(inc (rand-int d))))))

;; user=> (time (dorun (occur-count (take 1000000 (repeatedly #(roll 3 6))))))
;; "Elapsed time: 1674.796 msecs"

(defn occur-count2 [words]
  (let [res (atom {})]
    (doseq [w words]
      (let [v (int (@res w 0))]
        (swap! res assoc w (+ 1 v))))
    @res))
;; actually, this seems to work well in Clojure 1.4
;; user=> (time (dorun (occur-count2 (take 1000000 (repeatedly #(roll 3 6))))))
;; "Elapsed time: 1200.791 msecs"

(defn roll2 [n d]
  (let [p (int d)]
    (reduce + (take n (repeatedly #(inc (rand-int p)))))))
;; D'oh - worse...
;; user=> (time (dorun (occur-count2 (take 1000000 (repeatedly #(roll2 3 6))))))
;; "Elapsed time: 1322.332 msecs"

(defn roll3 [n d]
  (let [p (int d)]
    (reduce + (take n (repeatedly #(unchecked-inc (rand-int p)))))))
;; user=> (time (dorun (occur-count2 (take 1000000 (repeatedly #(roll3 3 6))))))
;; "Elapsed time: 1532.867 msecs"

(defn roll-final [n d]
  (loop [n (int n) sum 0]
    (if (zero? n)
      sum
      (recur (dec n) (+ sum (inc (rand-int d)))))))
;; performance.clj:161 recur arg for primitive local: sum is not matching primitive, had: Object, needed: long
;; Auto-boxing loop arg: sum

(defn occur-count-final [words]
  (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} words))
;; Ahhhhh...
;; user=> (time (dorun (occur-count-final (take 1000000 (repeatedly #(roll-final 3 6))))))
;; "Elapsed time: 541.179 msecs"

(defn mean
  "Takes a sequence of integers and returns their mean value"
  [sq]
  (let [length (int (count sq))]
    (if (zero? length)
      0
      (/ (int (reduce + sq)) length))))
;; user=> (mean [1 2 3 4 5])
;; 3
;; user=> (mean [1 2 3 4 5 6 7 8])
;; 9/2