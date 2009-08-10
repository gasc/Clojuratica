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
   :methods [[getExpr [] com.wolfram.jlink.Expr]]
   :extends clojure.lang.ASeq
   :implements [clojuratica.Flaggable]
   :init init
   :constructors {[Object] []
                  [Object Integer] []
                  [Object clojure.lang.IPersistentCollection] []}
   :state state)
  (:import [com.wolfram.jlink Expr MathLinkFactory])
  (:require [clojuratica.lib :as lib]))

(defn -first [this]
  (.head (:expr (.state this))))

(defn -next [this]
  (seq (.args (:expr (.state this)))))

(defn -getExpr [this]
  (:expr (.state this)))

(defn -getFlags [this]
  (:flags (.state this)))

(defn constructor-dispatch [& args]
  (letfn [(class-match? [classes] (lib/instances? classes args))]
    (cond (class-match? [Expr])                                     :expr
          (class-match? [Expr Integer])                             :expr+integer
          (class-match? [clojuratica.CExpr])                        :cexpr
          (class-match? [clojuratica.CExpr
                         clojure.lang.IPersistentCollection])       :cexpr+coll
          (class-match? [String])                                   :string
          (class-match? [Number])                                   :number
          (class-match? [clojure.lang.IPersistentCollection])       :coll
          (class-match? [Object])                                   :object
          (nil? (first args))                                       :nil
          true (throw (Exception. "Argument of invalid class passed to CExpr constructor: ")))))

(defmulti construct constructor-dispatch)

(defmethod construct :expr [expr]
  {:expr expr
   :flags '()})

(defmethod construct :expr+integer [expr pos]
  {:expr expr
   :flags '()})

(defmethod construct :cexpr [cexpr]
  (let [expr (.getExpr cexpr)]
    {:expr expr
     :flags (.getFlags cexpr)}))

(defmethod construct :cexpr+coll [cexpr coll]
  (let [expr (.getExpr cexpr)]
    {:expr expr
     :flags (concat coll (.getFlags cexpr))}))

(defmethod construct :string [s]
  (let [expr (Expr. s)]
    {:expr expr
     :flags '()}))

(defmethod construct :number [n]
  (let [typed-n (cond (instance? BigInteger n)         n
                      (instance? BigDecimal n)         n
                      (instance? Integer n)            (long n)
                      (instance? Short n)              (long n)
                      (instance? Long n)               n
                      (instance? Byte n)               (long n)
                      (instance? Double n)             n
                      (instance? Float n)              (double n)
                      (instance? clojure.lang.Ratio n) (double n)
                      true (throw (Exception. (str "CExpr constructor does not know how to handle number of class " (class n)))))
        expr          (Expr. typed-n)]
    {:expr expr
     :flags '()}))

(defmethod construct :coll [expression-coll]
  (let [loop (MathLinkFactory/createLoopbackLink)]
    (.putFunction loop "List" (count expression-coll))
    (dorun (for [expression expression-coll] (.put loop (.getExpr (clojuratica.CExpr. expression)))))
    (.endPacket loop)
    (let [expr (.getExpr loop)]
      {:expr expr
       :flags '()})))

(defmethod construct :object [obj]
  (let [loop (MathLinkFactory/createLoopbackLink)]
    (.put loop obj)
    (.endPacket loop)
    (let [expr (.getExpr loop)]
      {:expr expr
       :flags '()})))

(defmethod construct :nil [obj]
  (let [loop (MathLinkFactory/createLoopbackLink)]
    (.putSymbol loop "Null")
    (.endPacket loop)
    (let [expr (.getExpr loop)]
      {:expr expr
       :flags '()})))

(defn -init [& args]
  [[] (apply construct args)])
