(ns joy.large-scale-design
  (:refer-clojure :exclude [get]))

(defn beget [o p] (assoc o ::prototype p))
;; (beget {:sub 0} {:super 1})
;; => {:joy.udp/prototype {:super 1}, :sub 0}

(def put assoc)

(defn get [m k]
  (when m
    (if-let [[_ v] (find m k)]
      v
      (recur (::prototype m) k))))

;; (get (beget {:sub 0} {:super 1}) :super)
;; => 1

(def cat {:likes-dogs true :ocd-bathing true})
(def morris (beget {:likes-9lives true} cat))
(def post-traumatic-morris (beget {:likes-dogs nil} morris))
;; (get cat :likes-dogs)
;; => true
;; (get morris :likes-dogs)
;; => true
;; joy.udp=> (get post-traumatic-morris :likes-dogs)
;; => nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multimethods

(defmulti compiler :os)
(defmethod compiler ::unix [m] (get m :c-compiler))
(defmethod compiler ::osx [m] (get m :c-compiler))

(def clone (partial beget {}))
(def unix {:os         ::unix 
           :c-compiler "cc"
           :home       "/home"
           :dev        "/dev"})    
(def osx  (-> (clone unix)
              (put :os ::osx)
              (put :c-compiler "gcc")
              (put :home "/Users")))

(defmulti home :os)
(defmethod home ::unix [m] (get m :home))

(derive ::osx ::unix)
(derive ::osx ::bsd)
(defmethod home ::bsd [m] "/home")
(prefer-method home ::unix ::bsd)
;; (remove-method home ::bsd)
;; (home osx) ;=> "/Users"

(defmulti compile-cmd (juxt :os compiler))
(defmethod compile-cmd [::osx "gcc"] [m]
  (str "/usr/bin/" (get m :c-compiler)))
(defmethod compile-cmd :default [m]
  (str "Unsure where to locate " (get m :c-compiler)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types protocols and records

(defrecord TreeNode [val l r])

;; Add to tree
(defn xconj [t v]
  (cond
    (nil? t)       (TreeNode. v nil nil)
    (< v (:val t)) (TreeNode. (:val t) (xconj (:l t) v) (:r t))
    :else          (TreeNode. (:val t) (:l t) (xconj (:r t) v))))

(defn xseq [t]
  (when t
    (concat (xseq (:l t)) [(:val t)] (xseq (:r t)))))

(def sample-tree (reduce xconj nil [3 5 2 4 6]))
;; (xseq sample-tree)
;; => (2 3 4 5 6)

;; stacks
(defprotocol FIXO
  (fixo-push [fixo value])
  (fixo-pop [fixo])
  (fixo-peek [fixo]))

(extend-type TreeNode
  FIXO
  (fixo-push [node value]
    (xconj node value)))

;; (xseq (fixo-push sample-tree 5/2))
;; => (2 5/2 3 4 5 6)

(extend-type clojure.lang.IPersistentVector
  FIXO
  (fixo-push [vector value]
    (conj vector value)))
;; (fixo-push [2 3 4 5 6] 5/2)
;; => [2 3 4 5 6 5/2]

;; Clojure-style mixins (i.e. immutability extends to extended types too)
(require '[clojure.string :only [reverse upper-case] :as s])
(defprotocol StringOps (rev [s]) (upp [s]))
(extend-type String
  StringOps
  (rev [s] (s/reverse s)))
;; (rev "Works")
;; => "skroW"

(extend-type String
  StringOps
  (upp [s] (s/upper-case s)))
;; upp "Works")
;; => "WORKS"
;; (rev "Works")
;; => IllegalArgumentException No implementation of method: :rev of protocol: #'user/StringOps found for class: java.lang.String  clojure.core/-cache-protocol-fn (core_deftype.clj:527)

(def rev-mixin {:rev s/reverse})
(def upp-mixin {:upp (fn [this] (.toUpperCase this))})
(def fully-mixed (merge upp-mixin rev-mixin))
(extend String StringOps fully-mixed)
;; (-> "Works" upp rev)
;; => "SKROW"

;; Back to protocols.  This example demonstrates how to extend a protocol to nil(!)
;; (reduce fixo-push nil [3 5 2 4 6 0])
;; => IllegalArgumentException No implementation of method: :fixo-push of protocol: #'udp/FIXO found for class: nil  clojure.core/-cache-protocol-fn (core_deftype.clj:527)

(extend-type nil
  FIXO
  (fixo-push [t v]
    (TreeNode. v nil nil)))
;; (xseq (reduce fixo-push nil [3 5 2 4 6 0]))
;; => (0 2 3 4 5 6)

;; Complete implementation of FIXO for TreeNode and Vector
(extend-type TreeNode
  FIXO
  (fixo-push [node value]
    (xconj node value))
  (fixo-peek [node]
    (if (:l node)
      (recur (:l node))
      (:val node)))
  (fixo-pop [node]
    (if (:l node)
      (TreeNode. (:val node) (fixo-pop (:l node)) (:r node))
        (:r node))))

(extend-type clojure.lang.IPersistentVector
  FIXO
  (fixo-push [vector value]
    (conj vector value))
  (fixo-peek [vector]
    (peek vector))
  (fixo-pop [vector]
    (pop vector)))

;; (fixo-push (TreeNode. 1 nil nil) 2)
;; => #udp.TreeNode{:val 1, :l nil, :r #udp.TreeNode{:val 2, :l nil, :r nil}}

;; Sharing Method Implementations
(defn fixo-into [c1 c2]
  (reduce fixo-push c1 c2))
;; (xseq (fixo-into (TreeNode. 5 nil nil) [2 4 6 7]))
;; => (2 4 5 6 7)
;; (seq (fixo-into [5] [2 4 6 7]))
;; => (5 2 4 6 7)

; Using a map to extend FIXO to TreeNode
(def tree-node-fixo
  {:fixo-push (fn [node value]
                 (xconj node value))
   :fixo-peek (fn [node]
                 (if (:l node)
                   (recur (:l node))
                   (:val node)))
   :fixo-pop (fn [node]
                (if (:l node)
                  (TreeNode. (:val node) (fixo-pop (:l node)) (:r node))
                  (:r node)))})

(extend TreeNode FIXO tree-node-fixo)
;; (xseq (fixo-into (TreeNode. 5 nil nil) [2 4 6 7]))
;; => (2 4 5 6 7)

;; Size-limited stack FIXO using reify
(defn fixed-fixo
  ([limit] (fixed-fixo limit []))
  ([limit vector]
    (reify FIXO
      (fixo-push [this value]
        (if (< (count vector) limit)
          (fixed-fixo limit (conj vector value))
          this))
      (fixo-peek [_]
        (peek vector))
      (fixo-pop [_]
        (pop vector)))))

;; (-> (TreeNode. 1 nil nil) (fixo-push 2) (fixo-push 3) fixo-pop xseq)
;; => (2 3)

;; Method implementations using defrecord
(defrecord TreeNode [val l r]
  FIXO
  (fixo-push [t v]
    (if (< v val)
      (TreeNode. val (fixo-push l v) r)
      (TreeNode. val l (fixo-push r v))))
  (fixo-peek [t]
    (if l
      (fixo-peek l)
      val))
  (fixo-pop [t]
    (if l
      (TreeNode. val (fixo-pop l) r)
      r)))

;; (def sample-tree2 (reduce fixo-push (TreeNode. 3 nil nil) [5 2 4 6]))
;; (xseq sample-tree2)
;; => (2 3 4 5 6)

;; Simpler example
(defprotocol IBlah (print-a [t]))
(defrecord Blah [a b c]
  IBlah
  (print-a [t] (println a)))
;; (print-a (Blah. 1 2 3))
;; => 1
;; (instance? clojure.lang.Associative IBlah)
;; => true

(deftype InfiniteConstant [i]
  clojure.lang.ISeq
  (seq [this]
    (lazy-seq (cons i (seq this)))))
;; (take 3 (InfiniteConstant. 5))
;; => (5 5 5)
;; (:i (InfiniteConstant. 5))
;; => nil
;; (.i (InfiniteConstant. 5))
;; => 5

;; Final implementation of TreeNode
(deftype TreeNode [val l r]
  FIXO
  (fixo-push [_ v]
    (if (< v val)
      (TreeNode. val (fixo-push l v) r)
      (TreeNode. val l (fixo-push r v))))
  (fixo-peek [_]
    (if l
      (fixo-peek l)
      val))
  (fixo-pop [_]
    (if l
      (TreeNode. val (fixo-pop l) r)
      r))

  clojure.lang.IPersistentStack
  (cons [this v] (fixo-push this v))
  (peek [this] (fixo-peek this))
  (pop [this] (fixo-pop this))

  clojure.lang.Seqable
  (seq [t]
    (concat (seq l) [val] (seq r))))

(extend-type nil
  FIXO
  (fixo-push [t v]
    (TreeNode. v nil nil)))

;; (def sample-tree2 (into (TreeNode. 3 nil nil) [5 2 4 6]))
;; => #'joy-lsd/sample-tree2
;; (seq sample-tree2)
;; => (2 3 4 5 6)

;; Fluent Chess Move Builder in Clojure
(defn build-move [& pieces]
  (apply hash-map pieces))
;; (build-move :from "e7" :to "e8" :promotion \Q)
;; => {:from "e7", :to "e8", :promotion \Q}

(defrecord Move [from to castle? promotion]
  Object
  (toString [this]
    (str "Move " (:from this)
        " to "  (:to this)
        (if (:castle? this) " castle"
          (if-let [p (:promotion this)]
            (str " promote to " p)
            "")))))
;; (str (Move. "e2" "e4" nil nil))
;; => "Move e2 to e4"
;; (.println System/out (Move. "e7" "e8" nil \Q))
;; => Move e7 to e8 promote to Q
;; => nil

;; build-move implementation that separates concerns
;; "By wrapping the Move constructor in a build-move function, we put the
;; constructing moves there instead of in the type itself."
(defn build-move [& {:keys [from to castle? promotion]}]
  {:pre [from to]}
  (Move. from to castle? promotion))

;; (str (build-move :from "e2" :to "e4"))
;; => "Move e2 to e4"
;; (str (build-move :from "e2" :to nil))
;; => AssertionError Assert failed: to  user/build-move (NO_SOURCE_FILE:26)
