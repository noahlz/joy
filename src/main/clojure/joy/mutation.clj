(ns joy.mutation
  (:use joy.misc) ;; because we need the neighbors function
  (:require [clojure.xml :as xml]
             [clojure.zip :as zip])
  (:import [java.util.concurrent Executors]
            [java.util.regex Pattern]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When to use Refs
;; Also: See this helpful StackOverflow answer: http://stackoverflow.com/a/4999888/7507

(def ^:dynamic *pool* (Executors/newFixedThreadPool
                         (+ 2 (.availableProcessors (Runtime/getRuntime)))))

(defn dothreads! [f & {thread-count :threads
                       exec-count :times
                       :or {thread-count 1 exec-count 1}}]
  (dotimes [t thread-count]
    (.submit *pool* #(dotimes [_ exec-count] (f)))))

(def initial-board
  [[:- :k :-]
   [:- :- :-]
   [:- :K :-]])

(defn board-map [f bd]
  (vec (map #(vec (for [s %] (f s))) bd)))

(defn reset-board!
  "Resets the board state. Generally, these types of functions
   are a bad idea, but matters of page count force our hand."
  []
  (def board (board-map ref initial-board))
  (def to-move (ref [[:K [2 1]] [:k [0 1]]]))
  (def num-moves (ref 0)))

(def king-moves (partial neighbors
  [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]] 3))

(defn good-move? [to enemy-sq]
  (when (not= to enemy-sq) to))

(defn choose-move [[[mover mpos] [_ enemy-pos]]]
  [mover (some #(good-move? % enemy-pos)
               (shuffle (king-moves mpos)))])


(defn place [from to] to)

(defn move-piece [[piece dest] [[_ src] _]]
  (alter (get-in board dest) place piece)
  (alter (get-in board src ) place :-)
  (alter num-moves inc))

(defn update-to-move [move]
  (alter to-move #(vector (second %) move)))

(defn make-move []
  (dosync
    (let [move (choose-move @to-move)]
      (move-piece move @to-move)
      (update-to-move move))))

(defn go [move-fn threads times]
  (dothreads! move-fn :threads threads :times times))

;; user=> (reset-board!)
;; user=> (take 5 (repeatedly #(choose-move @to-move)))
;; ([:K [1 1]] [:K [1 1]] [:K [1 2]] [:K [2 2]] [:K [1 0]])
;; user=> (make-move)
;; [[:k [0 1]] [:K [1 1]]]
;; user=> (board-map deref board)
;; [[:- :k :-] [:- :K :-] [:- :- :-]]
;; user=> @num-moves
;; 1
;; user=> (board-map #(dosync (deref %)) board)
;; [[:k :- :-] [:- :- :-] [:K :- :-]]
;; user=> @to-move
;; [[:k [0 0]] [:K [2 0]]]
;; user=> @num-moves
;; 10001

;; redefine move-piece to use commute
(defn move-piece [[piece dest] [[_ src] _]]
  (commute (get-in board dest) place piece)
  (commute (get-in board src ) place :-)
  (commute num-moves inc))

(defn update-to-move [move]
  (commute to-move #(vector (second %) move)))

;; user=> (reset-board!)
;; #'joy.mutation/num-moves
;; user=> (go make-move 100 100)
;; nil
;; user=> (board-map deref board)
;; [[:- :- :k] [:K :- :-] [:- :- :-]]
;; user=> @to-move
;; [[:K [1 0]] [:k [0 2]]]

(defn stress-ref [r]
  (let [slow-tries (atom 0)]
    (future
      (dosync
        (swap! slow-tries inc)
        (Thread/sleep 200)
        @r)
      (println (format "r is: %s, history %d, after: %d tries"
                         @r (ref-history-count r) @slow-tries)))
    (dotimes [i 500]
      (Thread/sleep 10)
      (dosync (alter r inc)))
    :done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 11.3 Agents

(def log-agent (agent 0))

(defn do-log [msg-id message]
  (println msg-id ":" message)
  (inc msg-id))

(defn do-step [channel message]
  (Thread/sleep 1)
  (send-off log-agent do-log (str channel message)))

(defn three-step [channel]
  (do-step channel " ready to begin (step 0)")
  (do-step channel " warming up (step 1)")
  (do-step channel " really getting going now (step 2)")
  (do-step channel " done! (step 3)"))

(defn all-together-now []
  (dothreads! #(three-step "alpha"))
  (dothreads! #(three-step "beta"))
  (dothreads! #(three-step "omega")))

(defn exercise-agents [send-fn]
  (let [agents (map #(agent %) (range 10))]
    (doseq [a agents]
      (send-fn a (fn [_] (Thread/sleep 1000))))
    (doseq [a agents]
      (await a))))

(defn handle-log-error [the-agent the-err]
  (println "An action sent to the log agent threw " the-err))

(set-error-handler! log-agent handle-log-error)
(set-error-mode! log-agent :continue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 11.4 Atoms

(def ^:dynamic *time* (atom 0))
;; Equivalent: (def ^:dynamic *time* (java.util.concurrent.atomic.AtomicInteger. 0))
(defn tick [] (swap! *time* inc))
;; Equivalent: (defn tick [] (.getAndIncrement *time*))

;; A resettable memoize function
(defn manipulable-memoize [function]
  (let [cache (atom {})]
    (with-meta
      (fn [& args]
        (or (second (find @cache args))
          (let [ret (apply function args)]
            (swap! cache assoc args ret)
            ret)))
      {:cache cache})))

(def slowly (fn [x] (Thread/sleep 3000) x))
(def sometimes-slowly (manipulable-memoize slowly))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 11.5 Locks - See locks.clj

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 11.6 Futures

(defmulti rss-children class)
(defmethod rss-children String [uri-str]
  (-> (xml/parse uri-str) ;; Warning: blocks forever!
    zip/xml-zip
    zip/down
    zip/children))

(defn count-tweet-text-task [txt feed]
  (let [items (rss-children feed)
        re     (Pattern/compile (Pattern/quote txt))]
    (count
      (mapcat #(re-seq re (first %))
        (for [item (filter (comp #{:item} :tag) items)]
          (-> item :content first :content))))))

;; Macro that takes a set of tasks and converts them to a sequence of futures
(defmacro as-futures [[a args] & body]
  (let [parts            (partition-by #{'=>} body)
        [acts _ [res]]    (partition-by #{:as} (first parts))
        [_ _ task]        parts]
    `(let [~res (for [~a ~args] (future ~@acts))]
       ~@task)))

(defn tweet-occurrences [tag & feeds]
  (as-futures [feed feeds]
    (count-tweet-text-task tag feed)
      :as results
    =>
      (reduce (fn [total res] (+ total @res))
        0
        results)))

(defmacro with-promises [[n tasks _ as] & body]
  (when as
    `(let [tasks# ~tasks
           n# (count tasks#)
           promises# (take n# (repeatedly promise))]
       (dotimes [i# n#]
         (dothreads!
           (fn []
             (deliver (nth promises# i#)
                       ((nth tasks# i#))))))
       (let [~n tasks#
             ~as promises#]
         ~@body))))

(defrecord TestRun [run passed failed])
(defn pass [] true)
(defn fail [] false)

(defn run-tests [& all-tests]
  (with-promises
    [tests all-tests :as results]
    (into (TestRun. 0 0 0)
           (reduce #(merge-with + %1 %2) {}
             (for [r results]
               (if @r
                 {:run 1 :passed 1}
                 {:run 1 :failed 1}))))))
;; joy.mutation=> (run-tests pass fail fail fail pass)
;; #joy.mutation.TestRun{:run 5, :passed 2, :failed 3}

;; 11.7.2 Callback API to Blocking API
(defn tweet-items [k feed]
  (k
    (for [item (filter (comp #{:item} :tag) (rss-children feed))]
      (-> item :content first :content))))
;; ;; Try this later:
;; (tweet-tems count "http://twitter.com/statuses/user_timeline/46130870.rss")
;; ;; and this
;; (let [p (promise)]
;;   (tweet-items #(deliver p (count %))
;                 "http://twitter.com/statuses/user_timeline/46130870.rss")
;   @p)

;; Hmmm...isn't this kind of like Twitter Finagle?
(defmacro cps->fn [f k]
  `(fn [& args#]
     (let [p# (promise)]
       (apply ~f (fn [x#] (deliver p# (~k x#))) args#)
       @p#)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 11.8 Parallelism

(defn sleeper [s thing] (Thread/sleep (* 100 s)) thing)
(defn pvs [] (pvalues
               (sleeper 2 :1st)
               (sleeper 3 :2nd)
               (keyword "3rd")))

;; joy.mutation=> (-> (pvs) first time)
;; "Elapsed time: 202.311 msecs"
;; :1st
;; joy.mutation=> (->> [1 2 3] (pmap (comp inc (partial sleeper 2))) doall time)
;; "Elapsed time: 201.85 msecs"
;; (2 3 4)
;; joy.mutation=> (-> (pcalls #(sleeper 2 :1st) #(sleeper 3 :2nd) #(keyword "3rd")) doall time)
;; "Elapsed time: 305.005 msecs"
;; (:1st :2nd :3rd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 11.9 Vars

(defn print-read-eval []
  (println "*read-eval* is currently" *read-eval*))

(defn binding-play []
  (print-read-eval)
  (binding [*read-eval* false]
    (print-read-eval))
  (print-read-eval))

;; user=> (binding-play)
;; *read-eval* is currently true
;; *read-eval* is currently false
;; *read-eval* is currently true

;; user=> (def *bleah* true)
;; Warning: *bleah* not declared dynamic and thus is not dynamically rebindable, but its name suggests otherwise.
;;   Please either indicate ^:dynamic *bleah* or change the name. (NO_SOURCE_PATH:15)
;; user=> (binding [*bleah* false] (println *bleah*))
;; IllegalStateException Can't dynamically bind non-dynamic var: user/*bleah*
;;   clojure.lang.Var.pushThreadBindings (Var.java:353)


(defn test-with-precision []
  (with-precision 4
    (doall (map (fn [x] (/ x 3)) (range 1M 4M)))))

(defn test-bound-fn []
  (with-precision 4
    (map (bound-fn [x] (/ x 3)) (range 1M 4M))))
