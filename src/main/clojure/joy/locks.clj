(ns joy.locks
  (:refer-clojure :exclude [aget aset count seq])
  (:require [clojure.core :as clj]
             [joy.mutation :as mut])
  (:import [java.util.concurrent.locks ReentrantLock]))

(defprotocol SafeArray
  (aset   [this i f])
  (aget   [this i])
  (count  [this])
  (seq    [this]))

(defn make-dumb-array [t sz]
  (let [a (make-array t sz)]
    (reify
      SafeArray
      (count [_] (clj/count a))
      (seq [_] (clj/seq a))
      (aget [_ i] (clj/aget a i))
      (aset [this i f]
        (clj/aset a i (f (aget this i)))))))

(defn pummel [a]
  (mut/dothreads! #(dotimes [i (count a)] (aset a i inc)) :threads 100))
;; joy.locks=> (def D (make-dumb-array Integer/TYPE 8))
;; #'joy.locks/D
;; joy.locks=> (pummel D)
;; nil
;; joy.locks=> (seq D)
;; (30 80 62 81 69 70 78 83)


;; TODO: undestand why count and seq don't have locks...
(defn make-safe-array [t sz]
  (let [a (make-array t sz)]
    (reify
      SafeArray
      (count [_] (clj/count a))
      (seq [_] (clj/seq a))
      (aget [_ i]
        (locking a
          (clj/aget a i)))
      (aset [this i f]
        (locking a
          (clj/aset a i (f (aget this i))))))))
;; joy.locks=> (def A (make-safe-array Integer/TYPE 8))
;; #'joy.locks/A
;; joy.locks=> (pummel A)
;; nil
;; joy.locks=> (seq A)
;; (100 100 100 100 100 100 100 100)

(defn lock-i [target-index num-locks]
  (mod target-index num-locks))

;; Array that uses lock-striping (aqcuire a distinct lock to read/write an array slot)
(defn make-smart-array [t sz]
  (let [a           (make-array t sz)
        lock-size   (quot sz 2)
        lock-array  (into-array (take lock-size (repeatedly #(ReentrantLock.))))]
    (reify
      SafeArray
      (count [_] (clojure.core/count a))
      (seq [_] (clojure.core/seq a))
      (aget [_ i]
        (let [lk (clojure.core/aget lock-array (lock-i (inc i) lock-size))]
          (.lock lk)
          (try
            (clojure.core/aget a i)
            (finally (.unlock lk)))))
      (aset [this i f]
        (let [lk (clojure.core/aget lock-array (lock-i (inc i) lock-size))]
          (.lock lk)
          (try
            (clojure.core/aset a i (f (aget this i)))
            (finally (.unlock lk))))))))
;; joy.locks=> (def S (make-smart-array Integer/TYPE 8))
;; #'joy.locks/S
;; joy.locks=> (pummel S)
;; nil
;; joy.locks=> (seq S)
;; (100 100 100 100 100 100 100 10

