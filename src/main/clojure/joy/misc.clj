(ns joy.misc
  (:require [clojure.set]
             [clojure.xml :as xml]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joy of Clojure code snippets

(defn elevator [commands]
  (letfn
    [(ff-open [[cmd & r]]
       "When the elevator is open on the 1st floor
       it can either close or be done"
       #(case cmd
          :close (ff-closed r)
          :done true
          false))
     (ff-closed [[cmd & r]]
       "When the elevator is closed on the 1st floor
       it can either open or go up."
       #(case cmd
          :open (ff-open r)
          :up   (sf-closed r)
          false))
     (sf-closed [[cmd & r]]
       "When the elevator is closed on the 2nd floor
       it can either go down or open"
       #(case cmd
          :down (ff-closed r)
          :open (sf-open r)
          false))
     (sf-open [[cmd & r]]
       "When the elevator is open on the 2nd floor
       it can either close or be done"
       #(case cmd
          :close (sf-closed r)
          :done true
          false))]
    (trampoline ff-open commands)))


;; Adapted from: http://jakemccrary.com/blog/2010/12/06/trampolining-through-mutual-recursion/
(declare e? o?)

(defn my-even? [n]
   (trampoline e? n))

(defn e? [n]
  (if (zero? n)
    true
    #(o? (dec (Math/abs n)))))

(defn o? [n]
  (if (zero? n)
    false
    #(e? (dec (Math/abs n)))))

(defn fac-cps [n k]
  (letfn [(cont [v] (println k v n) (k (* v n)))]
    (if (zero? n)
      (k 1)
      (recur (dec n) cont))))

(defn fac [n]
  (fac-cps n identity))

(defn mk-cps [accept? end-value kend kont]
  (fn [n]
    ((fn [n k]
       (let [cont (fn [v] (k (kont v n)))]
         (if (accept? n)
           (k end-value)
           (recur (dec n) cont))))
       n kend)))

(def fac (mk-cps zero? 1 identity #(* %1 %2)))

(def tri (mk-cps zero? 1 dec #(+ %1 %2)))

;; Robot from Joy of Clojure, Ch 7

(def bearings [{:x  0 :y  1}   ; move north
               {:x  1 :y  0}   ; move east
               {:x  0 :y -1}   ; move south
               {:x -1 :y  0}]) ; move west

(defn forward [x y bearing-number]
  [(+ x (:x (bearings bearing-number)))
   (+ y (:y (bearings bearing-number)))])

(defn bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (bot (+ x (:x (bearings bearing-num)))
                          (+ y (:y (bearings bearing-num)))
                          bearing-num))
   :turn-right (fn [] (bot x y (mod (+ bearing-num 1) 4)))
   :turn-left  (fn [] (bot x y (mod (- bearing-num 1) 4)))})


;; Persistent Toy

(defn xconj [t v]
  (cond
    (nil? t)       {:val v :L nil :R nil}
    (< v (:val t)) {:val (:val t)
                     :L   (xconj (:L t) v)
                     :R   (:R t)}
    :else          {:val (:val t)
                     :L (:L t)
                     :R (xconj (:R t) v)}))

(defn xseq [t]
  (when t
    (concat (xseq (:L t)) [(:val t)] (xseq (:R t)))))


;; Laziness ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rec-step [[x & xs]]
  (if x
    [x (rec-step xs)]
    []))

(defn lz-rec-step [s]
  (lazy-seq
    (if (seq s)
      [(first s) (lz-rec-step (rest s))]
      [])))

(defn simple-range [i limit]
  (lazy-seq
    (when (< i limit)
      (cons i (simple-range (inc i) limit)))))

(defn triangle [n] (-> (+ n 1) (* n) (/ 2)))

(defn inf-triangles [n]
  {:head (triangle n)
   :tail (delay (inf-triangles (inc n)))})

(defn head [o] (:head o))
(defn tail [o] (force (:tail o)))

(def tri-nums (inf-triangles 1))
;; (->> tri-nums tail tail head)
;; => 6

(defn taker [n l]
  (loop [t n
         src l
         ret []]
    (if (zero? t)
      ret
      (recur (dec t) (tail src) (conj ret (head src))))))

(defn nthr [l n]
  (if (zero? n)
    (head l)
    (recur (tail l) (dec n))))

;; Joy of Clojure Lazy QuickSort
(defn nom [n] (take n (repeatedly #(rand-int n))))

(defn sort-parts [work]
  (lazy-seq
    (loop [[part & parts] work]
      (if-let [[pivot & xs] (seq part)]
        (let [smaller? #(< % pivot)]
          ;; (println "pivot=" pivot ", parts=" parts)
          (recur (list*
                   (filter smaller? xs)
                   pivot
                   (remove smaller? xs)
                   parts)))
          (when-let [[x & parts] parts]
            (cons x (sort-parts parts)))))))

(defn qsort [xs]
  (sort-parts (list xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A* Pathfinding from The Joy of Clojure

(def world [[1 1 1 1 1]
             [999 999 999 999 1]
             [1 1 1 1 1]
             [1 999 999 999 999]
             [1 1 1 1 1]])

(defn estimate-cost [step-cost-est size y x]
  (* step-cost-est
    (- (+ size size) y x 2)))

(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost
    (:cost cheapest-nbr 0)))

(defn total-cost [newcost step-cost-est size x y]
  (+ newcost
    (estimate-cost step-cost-est size x y)))

(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min this]
              (if (> (f min) (f this)) this min))
      coll)))

(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]] size yx))
  ([deltas size yx]
    (filter (fn [new-yx]
              (every? #(< -1 % size) new-yx))
      (map #(vec (map + yx %)) deltas)))) ;; NOTE: see http://joyofclojure.com/errata/

(defn astar [start-yx step-est cell-costs]
  (let [size (count cell-costs)]
    (loop [steps 0
           routes (vec (replicate size (vec (replicate size nil))))
           work-todo (sorted-set [0 start-yx])]
      (if (empty? work-todo)
        [(peek (peek routes)) :steps steps]
        (let [[_ yx :as work-item] (first work-todo)
              rest-work-todo (disj work-todo work-item)
              nbr-yxs (neighbors size yx)
              cheapest-nbr (min-by :cost
                                    (keep #(get-in routes %)
                                          nbr-yxs))
              newcost (path-cost (get-in cell-costs yx)
                                   cheapest-nbr)
              oldcost (:cost (get-in routes yx))]
          (if (and oldcost (>= newcost oldcost))
            (recur (inc steps) routes rest-work-todo)
            (recur (inc steps)
                   (assoc-in routes yx
                   {:cost newcost
                    :yxs (conj (:yxs cheapest-nbr [])
                                yx)})
              (into rest-work-todo
                     (map
                       (fn [w]
                         (let [[y x] w]
                           [(total-cost newcost step-est size y x) w]))
                       nbr-yxs)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 13.3 Design Patterns

(defmacro defformula [nm bindings & formula]
  `(let [~@bindings]
     (let [formula# (agent ~@formula)
           update-fn# (fn [key# ref# o# n#]
                         (send formula# (fn [_#] ~@formula)))]
       (doseq [r# ~(vec (take-nth 2 bindings))]
         (add-watch r# :update-formula update-fn#))
       (def ~nm formula#))))

;; user=> (def h (ref 25))
;; #'user/h
;; user=> (def ab (ref 100))
;; #'user/ab
;; user=> (defformula avg [at-bats ab hits h] (float (/ @hits @at-bats)))
;; #'user/avg
;; user=> @avg
;; 0.25
;; user=> (dosync (ref-set h 33))
;; 33
;; user=> @avg
;; 0.33

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 13.4 Error Handling and Debugging

(defn traverse [node f]
  (when node
    (f node)
    (doseq [child (:content node)]
      (traverse child f))))
;; user=> (traverse {:tag :flower :attrs {:name "Tanpopo"} :content []} println)
;; {:tag :flower, :attrs {:name Tanpopo}, :content []}

(def DB (-> "<zoo>
              <pongo>
                <animal>orangutan</animal>
              </pongo>
              <panthera>
                <animal>Spot</animal>
                <animal>lion</animal>
                <animal>Lopshire</animal>
              </panthera>
            </zoo>"
            .getBytes
            (java.io.ByteArrayInputStream.)
            xml/parse))

(defn ^{:dynamic true} handle-weird-animal
  [{[name] :content}]
  (throw (Exception. (str name " must be 'dealt with'"))))

(defmulti visit :tag)

(defmethod visit :animal [{[name] :content :as animal}]
  (case name
    "Spot"      (handle-weird-animal animal)
    "Lopshire"  (handle-weird-animal animal)
    (println name)))

(defmethod visit :default [node] nil)

;; user=> (traverse DB visit)
;; orangutan
;; Exception Spot must be 'dealt with'  joy.misc/handle-weird-animal (misc.clj:297)

(defmulti handle-weird (fn [{[name] :content}] name))

(defmethod handle-weird "Spot" [_]
  (println "Transporting Spot to the circus."))

(defmethod handle-weird "Lopshire" [_]
  (println "Signing Lopshire to a book deal."))

;; user=> (binding [handle-weird-animal handle-weird]
;;          (traverse DB visit))
;; orangutan
;; Transporting Spot to the circus.
;; lion
;; Signing Lopshire to a book deal.

;; Bind across thread for thread-specific handlers
;; user=> (def _ (future (binding [handle-weird-animal #(println (:content %))] (traverse DB visit))))
;; #'user/_
;; user=> orangutan
;; [Spot]
;; lion
;; [Lopshire]

