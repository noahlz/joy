(ns joy.debugging
  (:use [joy.macros :only [contextual-eval]]))

(defn readr [prompt exit-code]
  (let [input (clojure.main/repl-read prompt exit-code)]
    (if (= input ::tl) ;; note: you have to be in the joy.debugging namespace for this to work
      exit-code
      input)))

;; user=> (readr #(print "invisible=> ") ::exit)
;; [1 2 3]
;; [1 2 3]
;; user=> (readr #(print "invisible=> ") ::exit)
;; ::tl
;; :user/tl

(defmacro local-context []
  (let [symbols (keys &env)]
    (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))
;; user=> (local-context)
;; {}
;; user=> (let [a 1 b 2 c 3] (let [b 200] (local-context)))
;; {a 1, b 200, c 3}

(defmacro break []
  `(clojure.main/repl
     :prompt #(print "debug=> ")
     :read readr
     :eval (partial contextual-eval (local-context))))

(defn div [n d] (break) (int (/ n d)))

;; joy.debugging=> (div 10 0)
;; debug=> [n d]
;; [10 0]
;; debug=> ::tl
;; ArithmeticException Divide by zero  clojure.lang.Numbers.divide (Numbers.java:156)

(defn keys-apply [f ks m]
  (break)
  (let [only (select-keys m ks)]
    (break)
    (zipmap (keys only) (map f (vals only)))))
;; Ok, this doesn't work...
;; joy.debugging=> (keys-apply inc [:a :b] {:a 1 :b 2 :c 3})
;; debug=> only
;; CompilerException java.lang.RuntimeException: Unable to resolve symbol: only in this context,
;; compiling:(NO_SOURCE_PATH:20)
;; debug=> ks
;; [:a :b]
;; debug=> m
;; {:a 1, :c 3, :b 2}
;; debug=> ::tl
;; debug=> ::tl
;; {:a 2, :b 3}
;; joy.debugging=> only
;; CompilerException java.lang.RuntimeException: Unable to resolve symbol: only in this context,
;; compiling:(NO_SOURCE_PATH:0)
;; joy.debugging=> ::tl
;; :joy.debugging/tl

(defmacro awhen [expr & body]
  (break)
  `(let [~'it ~expr]
     (if ~'it
       (do (break) ~@body))))
;; joy.debugging=> (awhen [1 2 3] (it 2))
;; debug=> it
;; CompilerException java.lang.RuntimeException: Unable to resolve symbol: it in this context,
;; compiling:(NO_SOURCE_PATH:36)
;; debug=> expr
;; [1 2 3]
;; debug=> body
;; ((it 2))
;; debug=> ::tl
;; debug=>debug=> it
;; [1 2 3]
;; debug=> (it 1)
;; 2
;; debug=> ::tl
;; 3

