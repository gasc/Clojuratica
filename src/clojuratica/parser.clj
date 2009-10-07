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


(ns clojuratica.parser
  (:import [clojuratica CExpr]
           [com.wolfram.jlink Expr])
  (:use [clojuratica.lib]
        [clojuratica.core]
        [clojuratica.debug]))

(declare atom?
         primitive-array?
         primitive-vector?
         primitive-matrix?
         parse-atom
         parse-integer
         parse-rational
         parse-symbol
         parse-hash-map
         parse-list-to-lazy-seqs
         parse-list-to-vectors
         parse-primitive-atom
         parse-primitive-vector
         parse-primitive-matrix
         parse-generic-expression)

(defnf parse-dispatch [] []
  []
  [expression & _]
  (class expression))

(defmulti parse parse-dispatch)

(defmethod parse nil [& args]
  nil)

(defmethodf parse String [] []
  [_ passthrough-flags]
  [s & [kernel-link fn-wrap]]
  ; Takes a string and a KernelLink instance. Converts s to a CExpr using kernel-link, then parses the
  ; resulting CExpr into a Clojure object. Returns this Clojure object. For details on how CExprs
  ; are parsed see the documentation for the CExpr class.
  (if-not (instance? com.wolfram.jlink.KernelLink kernel-link)
    (throw (Exception. "When argument to parse is a string, the parser must have been created with a kernel-link argument.")))
  (apply parse (express s kernel-link) kernel-link fn-wrap passthrough-flags))

(defmethodf parse CExpr [] []
  [_ passthrough-flags]
  [cexpr & [_ fn-wrap]]
  (apply parse (.getExpr cexpr) nil fn-wrap (concat passthrough-flags (.getFlags cexpr))))

(defmethodf parse Expr [[:vectors :seqs]
                        [:fn-wrap :no-fn-wrap :auto-fn-wrap]]
                       [:seqs :auto-fn-wrap]
  [flags]
  [expr & [_ fn-wrap]]
  (if (and (flags :fn-wrap) (nil? fn-wrap))
    (throw (Exception. "Cannot parse functions using fn-wrap unless parser was created with a fn-wrap argument.")))
  (let [fn-wrap    (if (flags :no-fn-wrap) nil fn-wrap)]
    (cond (atom? expr) (apply parse-atom expr fn-wrap flags)
          (flags :seqs)
            (cond (primitive-vector? expr)  (parse-primitive-vector expr identity)
                  (primitive-matrix? expr)  (parse-primitive-matrix expr identity)
                  true                      (apply parse-list-to-lazy-seqs expr fn-wrap flags))
          (flags :vectors)
            (cond (primitive-vector? expr)  (parse-primitive-vector expr vec)
                  (primitive-matrix? expr)  (parse-primitive-matrix expr vec)
                  true                      (apply parse-list-to-vectors expr fn-wrap flags)))))

(defn needs-special-parser? [expr]
  (or (atom? expr) (primitive-array? expr)))

(defn atom? [expr] (not (.listQ expr)))

(defn primitive-array? [expr]
  (or (primitive-vector? expr) (primitive-matrix? expr)))

(defn primitive-vector? [expr]
  (cond (.vectorQ expr Expr/INTEGER)     Expr/INTEGER
        (.vectorQ expr Expr/BIGINTEGER)  Expr/BIGINTEGER
        (.vectorQ expr Expr/REAL)        Expr/REAL
        (.vectorQ expr Expr/BIGDECIMAL)  Expr/BIGDECIMAL
        (.vectorQ expr Expr/STRING)      Expr/STRING
        (.vectorQ expr Expr/RATIONAL)    Expr/RATIONAL
        (.vectorQ expr Expr/SYMBOL)      Expr/SYMBOL
        true                             false))

(defn primitive-matrix? [expr]
  (cond (.matrixQ expr Expr/INTEGER)     Expr/INTEGER
        (.matrixQ expr Expr/BIGINTEGER)  Expr/BIGINTEGER
        (.matrixQ expr Expr/REAL)        Expr/REAL
        (.matrixQ expr Expr/BIGDECIMAL)  Expr/BIGDECIMAL
        (.matrixQ expr Expr/STRING)      Expr/STRING
        (.matrixQ expr Expr/RATIONAL)    Expr/RATIONAL
        (.matrixQ expr Expr/SYMBOL)      Expr/SYMBOL
        true                             false))

(defn parse-primitive-vector [expr coll-fn & [type]]
  (with-debug-message (and debug (nil? type)) "vector parse"
    (let [type  (or type (primitive-vector? expr))
          v     (map #(parse-primitive-atom % type) (.args expr))]
      (coll-fn v))))

(defn parse-primitive-matrix [expr coll-fn & [type]]
  (with-debug-message debug "matrix parse"
    (let [type  (or type (primitive-matrix? expr))
          m     (map #(parse-primitive-vector % coll-fn type) (.args expr))]
      (coll-fn m))))

(defn parse-primitive-atom [expr type]
  (cond (= type Expr/BIGINTEGER)      (.asBigInteger expr)
        (= type Expr/BIGDECIMAL)      (.asBigDecimal expr)
        (= type Expr/INTEGER)         (parse-integer expr)
        (= type Expr/REAL)            (.asDouble expr)
        (= type Expr/STRING)          (.asString expr)
        (= type Expr/RATIONAL)        (parse-rational expr)
        (= type Expr/SYMBOL)          (parse-symbol expr)))

(defnf parse-hash-map [] []
  [_ passthrough-flags]
  [expr fn-wrap]
  (with-debug-message debug "hash-map parse"
    (let [[keys values] ((apply fn-wrap ["hm" expr] "Transpose[Apply[List, hm[], {1}]] &" passthrough-flags))]
      (zipmap keys values))))

(defnf parse-list-to-lazy-seqs [] []
  [_ passthrough-flags]
  [expr fn-wrap]
  (with-debug-message debug "parse of generic list into lazy-seq"
    (let [cexpr (convert expr)]
      (map #(apply parse % nil fn-wrap passthrough-flags) (rest cexpr)))))

(defnf parse-list-to-vectors [] []
  [_ passthrough-flags]
  [expr fn-wrap]
  (with-debug-message debug "parse of generic list into vector"
    (let [cexpr (convert expr)]
      (loop [elements (next cexpr)
             v        []
             stack    nil]
        (if-let [elements (seq elements)]
          (let [first-expr (first elements)]
            (if (needs-special-parser? first-expr)
              (recur (next elements) (conj v (apply parse first-expr nil fn-wrap passthrough-flags)) stack)
              (recur (next (convert first-expr)) [] (conj stack [(next elements) v]))))
          (if (seq stack)
            (let [[elements prior-v] (peek stack)]
              (recur elements (conj prior-v v) (pop stack)))
            v))))))

(defnf parse-atom [] []
  [_ passthrough-flags]
  [expr fn-wrap]
  (let [head (.toString (.head expr))]
    (cond (.bigIntegerQ expr)      (.asBigInteger expr)
          (.bigDecimalQ expr)      (.asBigDecimal expr)
          (.integerQ expr)         (parse-integer expr)
          (.realQ expr)            (.asDouble expr)
          (.stringQ expr)          (.asString expr)
          (.rationalQ expr)        (parse-rational expr)
          (.symbolQ expr)          (parse-symbol expr)
          (= "Function" head)      (if fn-wrap
                                     (apply fn-wrap [] expr passthrough-flags)
                                     (apply parse-generic-expression expr fn-wrap passthrough-flags))
          (= "HashMapObject" head) (if fn-wrap
                                     (apply parse-hash-map expr fn-wrap passthrough-flags)
                                     (apply parse-generic-expression expr fn-wrap passthrough-flags))
          true                     (apply parse-generic-expression expr fn-wrap passthrough-flags))))

(defn parse-integer [expr]
  (let [i (.asLong expr)]
    (if (and (<= i Integer/MAX_VALUE)
             (>= i Integer/MIN_VALUE))
      (int i)
      (long i))))

(defn parse-rational [expr]
  (let [numer (parse-integer (.part expr 1))
        denom (parse-integer (.part expr 2))]
    (/ numer denom)))

(defn parse-symbol [expr]
  (cond (= "True" (.toString expr))   true
        (= "False" (.toString expr))  false
        (= "Null" (.toString expr))   nil
        true                          (symbol (.toString expr))))

(defnf parse-generic-expression [] []
  [_ passthrough-flags]
  [expr fn-wrap]
  (cons (symbol (.toString (.head expr)))
        (apply parse (add-head "List" (.args expr))
                     nil
                     fn-wrap
                     passthrough-flags)))
