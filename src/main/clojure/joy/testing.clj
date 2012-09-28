(ns joy.testing
  (:use [joy.mutation] [clojure.core.contracts])
  (:require [clojure.test :as test]))

;; with-redefs and with-redefs-fn macro added in Clojure 1.3+

(defn tweetless-rss-children [s]
  '({:tag :title, :attrs nil, :content ["Stub"]}))

(defn count-rss2-children [s]
  (count (rss-children s)))

;; user=> (with-redefs [rss-children tweetless-rss-children]
;;          (count-rss2-children "dummy"))
;; 1

;; user=> (with-redefs [rss-children tweetless-rss-children]
;;         (tweet-occurrences "dummy" "test-url"))
;; 0

;; NOTE: can't just use "binding" because it is threadlocal, tests that
;; use threads (i.e. Futures) will fail.
;; Also it seems that binding won't work in 1.4 unless the var has ^dynamic meta-data
;; user=> (binding [rss-children tweetless-rss-children]
;;         (tweet-occurrences "dummy" "test-url"))
;; IllegalStateException Can't dynamically bind non-dynamic var: joy.mutation/rss-children  clojure.lang.Var.pushThreadBindings (Var.java:353)

(test/deftest feed-tests
  (with-redefs [rss-children tweetless-rss-children]
    (test/testing "RSS2 Child Counting"
      (test/is (= 1000 (count-rss2-children "dummy"))))
    (test/testing "Twitter Occurrence Counting"
      (test/is (= 0 (count-tweet-text-task "#clojure" ""))))))

(defn test-ns-hook []
  (feed-tests))

;; user=> (clojure.test/run-tests 'joy.testing)
;;
;; Testing joy.testing
;;
;; FAIL in (feed-tests) (testing.clj:31)
;; RSS2 Child Counting
;; expected: (= 1000 (count-rss2-children "dummy"))
;; actual: (not (= 1000 1))
;;
;; Ran 1 tests containing 2 assertions.
;; 1 failures, 0 errors.
;; {:type :summary, :pass 1, :test 1, :error 0, :fail 1}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preconditions and Postconditions

;; Note: contract is part of core.contracts in Clojure 1.3+
;; The below code is for Clojure 1.2
;; (def sqr (partial
;;   (contract sqr-contract
;;     [n]
;;     (require (number? n))
;;     (ensure (pos? %)))
;;   #(* % %)))

(def sqr
  (with-constraints
    (fn [n] (* n n))
    (contract sqr "requires a positive number"
      [x] [(and (pos? x) (number? x)) => (= (* x x) %)])))
;; NOTE: I tried to do something with (juxt) and (every?). It was less readable.
;; user=> (sqr 1)
;; 1
;; user=> (sqr -1)
;; AssertionError Pre-condition failure: requires a positive number
;; Assert failed: (and (pos? x) (number? x))  joy.testing/sqr/fn--1234 (testing.clj:66)


