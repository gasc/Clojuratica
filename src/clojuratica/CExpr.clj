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
   :methods [[getExpr [] com.wolfram.jlink.Expr]
             [getPos [] Integer]
             [parse [] Object]]
   :extends clojure.lang.ASeq
   :init init
   :constructors {[Object] []
                  [Object Object] []}
   :state state)
  (:import [com.wolfram.jlink])
  (:require [clojuratica.core] [clojuratica.lib]))

(defn -first [this]
  (let [cexpr (clojuratica.CExpr. (.. this getExpr (part (int-array (list (.getPos this))))))]
    (.parse cexpr)))

(defn -next [this]
  (let [expr   (.getExpr this)
        pos    (.getPos this)
        length (.length expr)]
    (if-not (== length pos)
      (clojuratica.CExpr. expr (inc pos)))))

(defn -getPos [this]
  (:pos (.state this)))

(defn -getExpr [this]
  (:expr (.state this)))

(defn -parse [this]
  (let [expr (.getExpr this)]
    (cond (.bigIntegerQ expr)         (.asBigInteger expr)
          (.bigDecimalQ expr)         (.asBigDecimal expr)
          (.integerQ expr)            (.asLong expr)
          (.realQ expr)               (.asDouble expr)
          (.stringQ expr)             (.asString expr)
          (.listQ expr)               (lazy-seq (next this))
          (= "Null" (.toString expr)) nil
          true                        expr)))

(defn constructor-dispatch [& args]
  (letfn [(class-match? [classes] (clojuratica.lib/instances? classes args))]
    (cond (class-match? [com.wolfram.jlink.Expr])              :expr
          (class-match? [com.wolfram.jlink.Expr Integer])      :expr+integer
          (class-match? [String])                              :string
          (class-match? [Number])                              :number
          (class-match? [clojure.lang.Seqable])                :seqable
          (class-match? [Object])                              :object
          true (throw (Exception. "Argument of invalid class passed to CExpr constructor: ")))))

(defmulti construct constructor-dispatch)

(defmethod construct :expr [expr]
  {:expr expr :pos 0})

(defmethod construct :expr+integer [expr pos]
  {:expr expr :pos pos})

(defmethod construct :string [s]
  {:expr (com.wolfram.jlink.Expr. s) :pos 0})

(defmethod construct :number [n]
  (let [typed-n (cond (instance? BigInteger n)         n
                      (instance? BigDecimal n)         n
                      (instance? Integer n)            (long n)
                      (instance? Short n)              (long n)
                      (instance? Byte n)               (long n)
                      (instance? Double n)             n
                      (instance? Float n)              (double n)
                      (instance? clojure.lang.Ratio n) (double n)
                      true (throw (Exception. (str "CExpr constructor does not know how to handle number of class " (class n)))))]
    {:expr (com.wolfram.jlink.Expr. typed-n) :pos 0}))

(defmethod construct :seqable [expression-seq]
  (let [loop (com.wolfram.jlink.MathLinkFactory/createLoopbackLink)]
    (.putFunction loop "List" (count expression-seq))
    (dorun (for [expression expression-seq] (.put loop (.getExpr (clojuratica.CExpr. expression)))))
    (.endPacket loop)
    {:expr (.getExpr loop) :pos 0}))

(defmethod construct :object [obj]
  (let [loop (com.wolfram.jlink.MathLinkFactory/createLoopbackLink)]
    (.put loop obj)
    (.endPacket loop)
    {:expr (.getExpr loop) :pos 0}))

(defn -init
  ([& args] [[] (apply construct args)]))
