;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DynaFrame
(ns joy.gui.DynaFrame
  (:gen-class
    :name           joy.gui.DynaFrame
    :extends        javax.swing.JFrame
    :implements    [clojure.lang.IMeta]
    :prefix         df-
    :state          state
    :init           init
    :constuctors   {[String] [String]}
    :methods        [[display [java.awt.Container] void]
                     ^{:static true} [version [] String]])
  (:import (javax.swing JFrame JPanel)
            (java.awt BorderLayout Container)))

;; In a "fresh REPL"
;; (compile 'joy.gui.DynaFrame)
;; => joy.gui.DynaFrame
;; (joy.gui.DynaFrame. "1st")
;; => UnsupportedOperationException joy.gui.DynaFrame/df-init not defined  joy.gui.DynaFrame.<init> (:-1)
;; (in-ns 'joy.gui.DynaFrame)
;; => #<Namespace joy.gui.DynaFrame>
(defn df-init [title] [[title] (atom {::title title})])
;; => #'joy.gui.DynaFrame/df-init
;; joy.gui.DynaFrame=> (joy.gui.DynaFrame. "2nd")
;; => UnsupportedOperationException meta (joy.gui.DynaFrame/df-meta not defined?)  joy.gui.DynaFrame.meta (:-1)
(defn df-meta [this] @(.state this))
;; => #'joy.gui.DynaFrame/df-meta
(defn version [] "1.0")
;; => #'joy.gui.DynaFrame/version
;; (meta (joy.gui.DynaFrame. "3rd"))
;; => {:joy.gui.DynaFrame/title "3rd"}
;; (joy.gui.DynaFrame/version)
;; => "1.0"

;; NOTE: this defines the ".display()" function implemented by this class, and the
;; the "df-" prefix is there because of the ":prefix" option in the gen-class statement above.
(defn df-display [this pane]
  (doto this
    (-> .getContentPane .removeAll)
    (.setContentPane (doto (JPanel.)
                         (.add pane BorderLayout/CENTER)))
    (.pack)
    (.setVisible true)))

;; NOTE: I had to run this in a lein repl, could not get it working in the IntelliJ "Clojure Console"
;; (def gui (joy.gui.DynaFrame. "4th"))
;; => #'joy.gui.DynaFrame/gui
;; (.display gui (doto (javax.swing.JPanel.) (.add (javax.swing.JLabel. "blah"))))
;; => nil
;; (.display gui (doto (javax.swing.JPanel.) (.add (javax.swing.JLabel. "TEST TEST TEST TEST"))))
;; => nil
