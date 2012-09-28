(ns joy.multimethods
  (:import (java.util Calendar)))

;; Using keyword as a function
(defmulti color :color)
(defmethod color "blue" [m] (println (:color m)))
(defmethod color "red" [m] (println (:color m)))
(defmethod color "yellow" [m] (println (:color m)))
(defmethod color :default [m] (println "unknown color."))

(color {:color "blue"})    ;=> "blue"
(color {:color "fuchsia"}) ;=> "unknown color."

;; Using a function
(defn- resolve-taxbracket
  [data]
  (when-let [income (:netincome data)]
    (cond
      (>= income 250000)
        :upper
      (and (>= income 20000) (< income 250000))
        :middle
      (< income 20000)
        :lower )))

(defmulti taxbracket resolve-taxbracket)
(defmethod taxbracket :upper [data] (println "you've been audited!"))
(defmethod taxbracket :middle [data] (println "you still exist?"))
(defmethod taxbracket :lower [data] (println "lucky ducky!"))

(taxbracket {:netincome 1000000}) ;=> audited.
(taxbracket {:netincome 17500})   ;=> lucky ducky!

;; Using juxt to generate a vector value that is mapped to a defmethod
(defn is-tuesday [& _] ; varargs, ignored 
  (= Calendar/TUESDAY (-> (Calendar/getInstance) (.get Calendar/DAY_OF_WEEK))))

(defmulti using-juxt (juxt :color resolve-taxbracket is-tuesday))
(defmethod using-juxt ["blue" :upper false] [data]
  (println "Taxes for color blue, upper bracket, on Tuesday."))
(defmethod using-juxt ["red" :lower true] [data]
  (println "Taxes for color red, lower bracket, not Tuesday."))
(defmethod using-juxt :default [data]
  (println "Please contact the IRS:" data))

(using-juxt {:netincome 275000 :color "blue"})
(using-juxt {:netincome  15000 :color "red"})

