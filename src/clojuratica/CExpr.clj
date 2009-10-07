; ***** BEGIN LICENSE BLOCK *****
; Version: MPL 1.1/GPL 2.0/LGPL 2.1
;
; The contents of this file are subject to the Mozilla Public License Version
; 1.1 (the "License"); you may not use this file except in compliance with
; the License. You may obtain a copy of the License at
; http://www.mozilla.org/MPL/
;
; Software distributed under the License is distributed on an "AS IS" basis,
; WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
; for the specific language governing rights and limitations under the
; License.
;
; The Original Code is the Clojure-Mathematica interface library Clojuratica.
;
; The Initial Developer of the Original Code is Garth Sheldon-Coulson.
; Portions created by the Initial Developer are Copyright (C) 2009
; the Initial Developer. All Rights Reserved.
;
; Contributor(s):
;
; Alternatively, the contents of this file may be used under the terms of
; either the GNU General Public License Version 2 or later (the "GPL"), or
; the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
; in which case the provisions of the GPL or the LGPL are applicable instead
; of those above. If you wish to allow use of your version of this file only
; under the terms of either the GPL or the LGPL, and not to allow others to
; use your version of this file under the terms of the MPL, indicate your
; decision by deleting the provisions above and replace them with the notice
; and other provisions required by the GPL or the LGPL. If you do not delete
; the provisions above, a recipient may use your version of this file under
; the terms of any one of the MPL, the GPL or the LGPL.
;
; ***** END LICENSE BLOCK *****
;

(ns clojuratica.CExpr
  (:gen-class
   :methods [[getExpr  [] com.wolfram.jlink.Expr]
             [getFlags [] clojure.lang.IPersistentCollection]]
   :extends clojure.lang.ASeq
   :init init
   :constructors {[Object] [],
                  [Object clojure.lang.IPersistentCollection] []}
   :state state)
  (:import [com.wolfram.jlink Expr MathLinkFactory])
  (:use [clojuratica.lib]
        [clojuratica.debug]))

(declare construct)

(defn -init [& args]
  [[] (apply construct args)])

(defn -first [this]
  (.head (:expr (.state this))))

(defn -next [this]
  (seq (.args (:expr (.state this)))))

(defn -getExpr [this]
  (:expr (.state this)))

(defn -getFlags [this]
  (:flags (.state this)))

(defmulti construct (fn [& args] (vec (map class args))))

(defmethod construct [Expr]
  [expr]
  {:expr expr
   :flags '()})

(defmethod construct [clojuratica.CExpr]
  [cexpr]
  (.state cexpr))

(defmethod construct [clojuratica.CExpr clojure.lang.IPersistentCollection]
  [cexpr coll]
  {:expr (-getExpr cexpr)
   :flags (concat coll (-getFlags cexpr))})

(defmethod construct [Object]
  [obj]
  (let [loop (MathLinkFactory/createLoopbackLink)]
    (.put loop obj)
    (.endPacket loop)
    (let [expr (.getExpr loop)]
      (construct expr))))

(defn needs-special-constructor? [element]
  (or (instance? clojure.lang.Ratio element)
      (instance? clojure.lang.IPersistentMap element)
      (instance? clojure.lang.Sequential element)
      (instance? clojure.lang.Symbol element)
      (nil? element)))

(defmethod construct [clojure.lang.Ratio]
  [n]
  (let [loop (MathLinkFactory/createLoopbackLink)]
    (.putFunction loop "Rational" 2)
    (.put loop (.numerator n))
    (.put loop (.denominator n))
    (.endPacket loop)
    (let [expr (.getExpr loop)]
      (construct expr))))

(defmethod construct [clojure.lang.IPersistentMap]
  [expression-map]
  (let [loop (MathLinkFactory/createLoopbackLink)]
    (.putFunction loop "HashMap" (count expression-map))
    (doseq [[key value] expression-map]
      (.putFunction loop "Rule" 2)
      (.put loop (:expr (construct key)))
      (.put loop (:expr (construct value))))
    (.endPacket loop)
    (let [expr (.getExpr loop)]
      (construct expr))))

(defmethod construct [clojure.lang.Symbol]
  [expression-symbol]
  (if (re-find #"[^a-zA-Z0-9`]" (name expression-symbol))
    (throw (Exception. "Symbols passed to Mathematica must be alphanumeric (apart from backticks)")))
  (construct (Expr. Expr/SYMBOL (name expression-symbol))))

(defn mvector? [coll]
  (and (sequential? coll)
       (not-any? needs-special-constructor? coll)))

(defn mmatrix? [coll]
  (and (sequential? coll)
       (every? mvector? coll)))

(defn sequential-to-array [coll]
  (cond (mmatrix? coll)  (do (if debug (println "Converting matrix...")) (to-array-2d coll))
        (mvector? coll)  (do (if debug (println "Converting vector...")) (to-array coll))
        true             (to-array
                           (map #(cond (sequential? %)                (sequential-to-array %)
                                       (needs-special-constructor? %) (:expr (construct %))
                                       true                           %)
                                coll))))

(defmethod construct [clojure.lang.Sequential]
  [expression-coll]
  (construct (sequential-to-array expression-coll)))

(defmethod construct [nil]
  [_]
  (let [loop (MathLinkFactory/createLoopbackLink)]
    (.putSymbol loop "Null")
    (.endPacket loop)
    (let [expr (.getExpr loop)]
      (construct expr))))

;(defmethod construct [Number]
;  [n]
;  (let [typed-n (cond (instance? BigInteger n)         n
;                      (instance? BigDecimal n)         n
;                      (instance? Integer n)            (long n)
;                      (instance? Short n)              (long n)
;                      (instance? Long n)               n
;                      (instance? Byte n)               (long n)
;                      (instance? Double n)             n
;                      (instance? Float n)              (double n)
;                      (instance? clojure.lang.Ratio n) (double n)
;                      true (throw (Exception. (str "CExpr constructor does not know how to handle number of class " (class n)))))
;        expr          (Expr. typed-n)]
;    {:expr expr
;     :flags '()}))

;(defmethod construct [clojure.lang.Sequential]
;  [expression-coll]
;  (let [loop (MathLinkFactory/createLoopbackLink)]
;    (.putFunction loop "List" (count expression-coll))
;    (doseq [expression expression-coll]
;      (.put loop (.getExpr (clojuratica.CExpr. expression))))
;    (.endPacket loop)
;    (let [expr (.getExpr loop)]
;      {:expr expr
;       :flags '()})))

;(defmethod construct [(class (double-array [1.0]))]
;  [expression-array]
;  (println "double constructor")
;  {:expr (Expr. expression-array)
;   :flags '()})

;(defmethod construct [(class (int-array [1]))]
;  [expression-array]
;  (println "int constructor")
;  {:expr (Expr. expression-array)
;   :flags '()})


;(defmethod construct [String]
;  [s]
;  (let [expr (Expr. s)]
;    {:expr expr
;     :flags '()}))

