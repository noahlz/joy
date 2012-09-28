(ns joy.dsl
  (:require [clojure.set :as ra]))

(def artists
  #{{:artist "Burial" :genre-id 1}
    {:artist "Magma"  :genre-id 2}
    {:artist "Can"    :genre-id 3}
    {:artist "Faust"  :genre-id 4}
    {:artist "Ikonika" :genre-id 1}
    {:artist "Grouper"}})

(def genres
  #{{:genre-id 1 :genre-name "Dubstep" }
     { :genre-id 2 :genre-name "Zeuhl" }
     { :genre-id 3 :genre-name "Prog" }
     { :genre-id 4 :genre-name "Drone" }})

(def ALL identity)

;; user=> (require '[clojure.set :as ra])
;; nil
;;
;; user=> (ra/select ALL genres)
;;   #{{:genre-id 4, :genre-name "Drone"} {:genre-id 3, :genre-name "Prog"} {:genre-id 2, :genre-name "Zeuhl"} {:genre-id 1, :genre-name "Dubstep"}}
;;
;; user=> (ra/select #(#{1 3} (:genre-id %)) genres)
;;   #{{:genre-id 3, :genre-name "Prog"} {:genre-id 1, :genre-name "Dubstep"}}
;;
;; user=> (take 2 (ra/select ALL (ra/join artists genres)))
;; ({:genre-id 1, :genre-name "Dubstep", :artist "Burial"} {:genre-id 2, :genre-name "Zeuhl", :artist "Magma"})

(defn meters->feet [m] (* m 3.28083989501312))
(defn meters->miles [m] (* m 0.000621))
;; user=> (meters->miles 1609.344)
;; 0.999402624

(defn relative-units [u units]
  (let [spec (u units)]
    (if (nil? spec)
      (throw (Exception. (str "Undefined unit " u)))
      (if (vector? spec)
        (let [[conv to] spec]
          (* conv (relative-units to units)))
        spec))))

;; user=> (relative-units :m {:m 1 :cm 100 :mm  [10 :cm]})
;; 1
;; user=> (relative-units :cm {:m 1 :cm 100 :mm [10 :cm]})
;; 100
;; user=> (relative-units :mm {:m 1 :cm 100 :mm [10 :cm]})
;; 1000

;; user=> (relative-units :mile {:ft 1 :mile [5000 :ft]})
;; 5000
;; user=> (relative-units :yard {:ft 1 :yard [3 :ft] :mile [5000 :ft]})
;; 3

;; TODO: shave head, move to a cave in the mountains, and ponder this macro for 3 years
(defmacro defunits-of [name base-unit & conversions]
  (let [magnitude (gensym)
        unit (gensym)
        units-map (into `{~base-unit 1}
                    (map vec (partition 2 conversions)))]
    `(defmacro ~(symbol (str "unit-of-" name))
       [~magnitude ~unit]
       `(* ~~magnitude
           ~(case ~unit
              ~@(mapcat
                  (fn [[u# & r#]]  ;; errata. Book is missing outer closing ]
                      `[~u# ~(relative-units u# units-map)])
                       units-map))))))

(defunits-of distance :m
  :km 1000
  :cm 1/100
  :mm [1/10 :cm]
  :ft 0.3048
  :mile [5280 :ft])

;; user=> (into {:m 1} (map vec (partition 2 '(:km 1000 :cm 1/100 :mm [1/10 :cm] :ft 0.3048 :mile [5280 :ft]))))
;; {:m 1, :km 1000, :cm 1/100, :mm [1/10 :cm], :ft 0.3048, :mile [5280 :ft]}

;; http://stackoverflow.com/questions/12573308/how-to-write-a-nested-macro-that-prints-its-own-suffix

;; user=> (clojure.walk/macroexpand-all
;;          '(defunits-of distance :m
;;             :km 1000
;;             :cm 1/100
;;             :mm [1/10 :cm]
;;             :ft 0.3048
;;             :mile [5280 :ft]))
;; (do (def unit-of-distance (fn* ([&form &env G__69 G__70] (clojure.core/seq (clojure.core/concat (clojure.core/list (quote clojure.core/*)) (clojure.core/list G__69) (clojure.core/list (let* [G__71 G__70] (case* G__71 6 7 (throw (new java.lang.IllegalArgumentException (clojure.core/str "No matching clause: " G__71))) {0 [:mm 1/1000], 1 [:ft 0.3048], 2 [:km 1000], 3 [:cm 1/100], 4 [:m 1], 6 [:mile 1609.344]} :compact :hash-identity nil)))))))) (. (var unit-of-distance) (setMacro)) (var unit-of-distance))
;; NOTE: Whoa

;; user=> (macroexpand '(unit-of-distance 1 :cm))
;; (clojure.core/* 1 1/100)


