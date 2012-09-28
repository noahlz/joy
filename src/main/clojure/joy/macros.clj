(ns joy.macros)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joy of Clojure, Chapter 8 - Macros

(defn contextual-eval [ctx expr]
  (eval
    `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
       ~expr)))

;; Handling nested syntax-quotes o_O
(defn nested-quotes []
  (let [x 9, y '(- x)]
    (println `y)
    (println ``y)
    (println ``~y)
    (println ``~~y)
    (contextual-eval {'x 36} ``~~y)))

(defmacro do-until [& clauses]
  (when clauses
    (list `when (first clauses)
            (if (next clauses)
              (second clauses)
              (throw (IllegalArgumentException.
                       "do-until rquires an even number of forms")))
      (cons 'do-until (nnext clauses)))))

(defmacro unless [condition & body]
  `(if (not ~condition)              ;; Unquote condition
     (do ~@body)))                   ;; Splice body

(defn from-end [s n]
  (let [delta (dec (- (count s) n))]
    (unless (neg? delta)             ;; return nil if negative
      (nth s delta))))

(defmacro def-watched [name & value]
  `(do
     (def ~name ~@value)
     (add-watch (var ~name)
                 :re-bind
                 (fn [~'key ~'r old# new#]
                   (println old# " -> " new#)))))

; (def-watched x 2)
;=> #'user/x
; (def x 0)
;=> 2 -> 0
;=> #'user/x

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Domain macro

(defmacro domain [name & body]
  `{:tag :domain
    :attrs {:name (str '~name)}
    :content [~@body]})

(declare handle-things)

(defmacro grouping [name & body]
  `{:tag :grouping
    :attrs {:name (str '~name)}
    :content [~@(handle-things body)]})

(declare grok-attrs grok-props)

(defn handle-things [things]
  (for [t things]
    {:tag :thing
     :attrs (grok-attrs (take-while (comp not vector?) t))
     :content (if-let [c (grok-props (drop-while (comp not vector?) t))]
                [c]
                [])}))

(defn grok-attrs [attrs]
  (into {:name (str (first attrs))}
    (for [a (rest attrs)]
      (cond
        (list? a) [:isa (str (second a))]
        (string? a) [:comment a]))))

(defn grok-props [props]
  (when props
    {:tag :properties
     :attrs nil
     :content (apply vector (for [p props]
                  {:tag :property
                   :attrs {:name (str (first p))}
                   :content nil}))}))

(def d
  (domain man-vs-monster
    (grouping people
      (Human "A stock human")

      (Man (isa Human)
        "A man, baby"
        [name]
        [has-beard?]))

    (grouping monsters
      (Chupacabra
        "A fierce, yet elusive creature"
        [eats-goats?]))))

;; To examine this domain as XML, use
;; (use '[clojure.xml :as xml])
;; (xml/emit d)

;; End domain macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Anaphora macro
(defmacro awhen [expr & body]
  `(let [~'it ~expr]
     (when ~'it
       (do ~@body))))

;; (awhen [:a :b :c] (second it))
;; => :b

;; Generic close resources macro
(defmacro with-resource [binding close-fn & body]
  `(let ~binding
     (try
       (do ~@body)
       (finally
         (~close-fn ~(binding 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros returning functions
(declare collect-bodies)

(defmacro contract [name & forms]
  (list* `fn name (collect-bodies forms)))

(fn doubler
  ([f x]
    {:post [(= (* 2 x) %)]
     :pre [(pos? x)]}
     (f x)))

(declare build-contract)

(defn collect-bodies [forms]
  (for [form (partition 3 forms)]
    (build-contract form)))

(defn build-contract [c]
  (let [args (first c)]
    (list
      (into '[f] args)
      (apply merge
        (for [con (rest c)]
          (cond
            (= (first con) :require)
              (assoc {} :pre (vec (rest con)))
            (= (first con) :ensure)
              (assoc {} :post (vec (rest con)))
            :else
              (throw (Exception. (str "Unknown tag " (first con)))))))
    (list* 'f args))))

(def doubler-contract
  (contract doubler
    [x]
    (:require
      (pos? x))
    (:ensure
      (= (* 2 x) %))))

(def times2 (partial doubler-contract #(* 2 %)))
;; (times2 9)
;; => 18

(def times3 (partial doubler-contract #(* 3 %)))
;; (times3 9)
;; => AssertionError Assert failed: (= (* 2 x) %)  user/doubler (NO_SOURCE_FILE:33)
;; Fails because the contract states that the result must be double the input.

;; multi-arity version
(def doubler-contract
  (contract doubler
    [x]
      (:require
        (pos? x))
      (:ensure
         (= (* 2 x) %))
    [x y]
      (:require
        (pos? x)
        (pos? y))
      (:ensure
        (= (* 2 (+ x y)) %))))

;; ((partial doubler-contract #(* 2 (+ %1 %2))) 2 3)
;; => 10

;; ((partial doubler-contract #(+ %1 %2 %1 %2)) 2 3)
;; => 10

;; ((partial doubler-contract #(* 3 (+ %1 %2))) 2 3)
;; => AssertionError Assert failed: (= (* 2 (+ x y)) %)  user/doubler (NO_SOURCE_FILE:66)
