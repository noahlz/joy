(ns joy.java-next
  (:import (com.sun.net.httpserver HttpHandler HttpExchange HttpServer)
            (java.net InetSocketAddress HttpURLConnection)
            (java.io IOException FilterOutputStream)
            (java.util Arrays)))

;; "Flowchart for choosing the right Clojure type definition form"
;; http://cemerick.com/2011/07/05/flowchart-for-choosing-the-right-clojure-type-definition-form/
;; "Why does Clojure have 5 ways to define a class instead of just one?"
;; http://stackoverflow.com/questions/7142495/why-does-clojure-have-5-ways-to-define-a-class-instead-of-just-one

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple dynamic web service
(defn new-server [port path handler]
  (doto (HttpServer/create (InetSocketAddress. port) 0)
    (.createContext path handler)
    (.setExecutor nil)
    (.start)))

(defn default-handler [txt]
  (proxy [HttpHandler] []
    (handle [exchange]
      (.sendResponseHeaders exchange HttpURLConnection/HTTP_OK 0)
      (doto (.getResponseBody exchange)
        (.write (.getBytes txt))
        (.close)))))

;; (def server (new-server 8123 "/joy/hello" (default-handler "Hello Cleveland")))
;; => Point web browser to http://localhost:8123/joy/localhost
;; (.stop server 0)

(def p (default-handler
         "There's no problem that can't be solved with another level of indirection"))

;; (def server (new-server 8123 "/joy/hello" p))
;; => Now site displays text defined by p
;; (change-message p "Our new message")

(defn make-handler-fn [fltr txt]
  (fn [this exchange]
    (let [b (.getBytes txt)]
      (-> exchange
        .getResponseHeaders
        (.set "Content-Type" "text/html"))
      (.sendResponseHeaders exchange
                            HttpURLConnection/HTTP_OK
                            0)
      (doto (fltr (.getResponseBody exchange))
        (.write b)
        (.close)))))

(defn change-message
  "Convenience method to change a proxy's output message"
  ([p txt] (change-message p identity txt))
  ([p fltr txt]
    (update-proxy p
      {"handle" (make-handler-fn fltr txt)})))

(defn screaming-filter [o]
  (proxy [FilterOutputStream] [o]
    (write [b]
      (proxy-super write (.getBytes (str "<strong>"
                                       (.toUpperCase (String. b))
                                       "</strong>"))))))
;; (change-message p screaming-filter "ALL CAPS AND &LT;STRONG&GT; LOOKS LIKE YELLING")
;; => #<Object$HttpHandler$1e48143f user.proxy$java.lang.Object$HttpHandler$1e48143f@126d2380>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see joy/gui/DynaFrame.clj and joy/gui/socks.clj for next examples

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java arrays

(defn make-primitive-array []
  (let [ary (make-array Integer/TYPE 3 3)]
    (dotimes [i 3]
      (dotimes [j 3]
        (aset ary i j (int (+ i j)))))     ;; NOTE: (int (+ 1 j)) fixed errata here.
    (map seq ary)))

(defn asum-sq [xs]
  (let [dbl (amap xs i ret
               (* (aget xs i)
                  (aget xs i)))]
    (areduce dbl i ret 0
      (+ ret (aget dbl i)))))

;; NOTE: Joy of Clojure used (float-array) but must use into-array in 1.4
;; (asum-sq (into-array [1 2 3 4 5]))
;; => 55

(defmulti what-is class)
(defmethod what-is (Class/forName "[Ljava.lang.String;")  [a] "1d String")
(defmethod what-is (Class/forName "[[Ljava.lang.Object;") [a] "2d Object")
(defmethod what-is (Class/forName "[[[[I")                  [a] "Primitive 4d int")
(defmethod what-is (Class/forName "[[D")                    [a] "Primitive 2d double")
(defmethod what-is (Class/forName "[Lclojure.lang.PersistentVector;")
                                                                [a] "1d Persistent Vector")

(import '[java.util Comparator Collections ArrayList])
(defn gimme [] (ArrayList. [1 3 4 8 2]))
;; (doto (gimme) (Collections/sort (Collections/reverseOrder)))
;; => #<ArrayList [8, 4, 3, 2, 1]>

;; Don't do this! Use something like (doto (gimme) (Collections/sort <)) instead
(defn naive-reverse-comparator []
 (doto (gimme)
   (Collections/sort
     (reify Comparator
       (compare [this l r]
         (cond
           (> l r) -1
           (= 1 r) 0
           :else 1))))))
;; (naive-reverse-comparator)
;; => #<ArrayList [8, 4, 3, 2, 1]>

(defn my-shuffle [coll]
  (seq (doto (java.util.ArrayList. coll)
         java.util.Collections/shuffle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 10.6 definterface

(defprotocol Sliceable
  (slice [this s e1])
  (sliceCount [this]))

(definterface ISliceable
  (slice [^int s ^int e])
  (^int sliceCount []))

(def dumb
  (reify ISliceable
    (slice [_ s e] [:empty])
    (sliceCount [_] 42)))
;; user=> (.slice dumb 1 2)
;; [:empty]
;; user=> (.sliceCount dumb)
;; 42

(defn calc-slice-count [thing]
  "Calculates the number of posssible slices using the formula:
     (n + r - 1)!
     ------------
       r!(n - 1)
    where n is (count thing) and r is 2"
  (let [! #(reduce * (take % (iterate inc 1)))
        n (count thing)]
    (/ (! (- (+ n 2) 1))
      (* (! 2) (! (- n 1))))))

(extend-type String
  Sliceable
  (slice [this s e] (.substring this s (inc e)))
  (sliceCount [this] (calc-slice-count this)))
;; user=> (slice "abc" 0 1)
;; "ab"
;; user=> (sliceCount "abc")
;; 6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 10.7 Exceptions

(defn explode [] (explode))
;; user=> (try (explode) (catch Exception e "Stack is blown"))
;; StackOverflowError   user/explode (NO_SOURCE_FILE:25)
;; user=> (try (explode) (catch StackOverflowError e "Stack is blown"))
;; "Stack is blown"

(defmacro pairs [& args]
  (if (even? (count args))
    `(partition 2 '~args)
    (throw (Exception. (str "paris requires an even number of args")))))

(defmacro -?> [& forms]
  `(try (-> ~@forms)
     (catch NullPointerException _# nil)))
