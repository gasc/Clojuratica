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
        [clojuratica.core]))

(declare parse-atom
         parse-to-lazy-seqs
         parse-to-vectors)

(defnf parse-dispatch [] []
  []
  [expression & _]
  (class expression))

(defmulti parse parse-dispatch)

(defmethodf parse String [] []
  [_ passthrough-flags]
  [s & [kernel-link fn-wrap]]
  ; Takes a string and a KernelLink instance. Converts s to a CExpr using kernel-link, then parses the
  ; resulting CExpr into a Clojure object. Returns this Clojure object. For details on how CExprs
  ; are parsed see the documentation for the CExpr class.
  (if-not (instance? com.wolfram.jlink.KernelLink kernel-link)
    (throw (Exception. "When argument to parse is a string, the parser must have been created with a kernel-link argument.")))
  (apply parse (express s kernel-link) kernel-link fn-wrap passthrough-flags))

(defmethodf parse Expr [] []
  [_ passthrough-flags]
  [expr & [_ fn-wrap]]
  (apply parse (express expr) nil fn-wrap passthrough-flags))

(defmethodf parse CExpr [[:vectors :seqs]
                         [:fn-wrap :no-fn-wrap]]
                        (concat (.getFlags cexpr) [:seqs])
  [flags]
  [cexpr & [_ fn-wrap]]
  (if (and (flags :fn-wrap) (nil? fn-wrap))
    (throw (Exception. "Cannot parse functions using fn-wrap unless parser was created with an fn-wrap argument.")))
  (let [fn-wrap (if (flags :no-fn-wrap) nil fn-wrap)]
    (if (flags :vectors)
      (parse-to-vectors cexpr fn-wrap)
      (parse-to-lazy-seqs cexpr fn-wrap))))

(defmethod parse nil [& args]
  nil)

(defn parse-atom [cexpr fn-wrap]
  (let [expr (.getExpr cexpr)]
    (cond (.bigIntegerQ expr)                     (.asBigInteger expr)
          (.bigDecimalQ expr)                     (.asBigDecimal expr)
          (.integerQ expr)                        (.asLong expr)
          (.realQ expr)                           (.asDouble expr)
          (.stringQ expr)                         (.asString expr)
          (= "Null" (.toString expr))             nil
          (= "Function" (.toString (.head expr))) (if fn-wrap (fn-wrap [] expr) cexpr)
          true                                    cexpr)))

(defn parse-to-lazy-seqs [cexpr fn-wrap]
  (if-not (.listQ (.getExpr cexpr))
    (parse-atom cexpr fn-wrap)
    (let [parse-list-elements
            (fn parse-list-elements [s]
               (when (seq s)
                 (lazy-seq
                   (cons (parse-to-lazy-seqs (convert (first s)) fn-wrap)
                         (parse-list-elements (rest s))))))]
      (parse-list-elements (rest cexpr)))))

(defn parse-to-vectors [cexpr fn-wrap]  ; logic courtesy of Meikel Brandmeyer
  (if-not (.listQ (.getExpr cexpr))
    (parse-atom cexpr fn-wrap)
    (loop [elements (next cexpr)
           v        []
           stack    nil]
      (if-let [elements (seq elements)]
        (let [first-cexpr (convert (first elements))]
          (if-not (.listQ (.getExpr first-cexpr))
            (recur (next elements) (conj v (parse-atom first-cexpr fn-wrap)) stack)
            (recur (next first-cexpr) [] (conj stack [(next elements) v]))))
        (if (seq stack)
          (let [[elements prior-v] (peek stack)]
            (recur elements (conj prior-v v) (pop stack)))
          v)))))

