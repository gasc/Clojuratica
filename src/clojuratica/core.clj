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


(ns clojuratica.core
  (:import  [clojuratica CExpr]
            [com.wolfram.jlink Expr])
  (:use     [clojuratica.lib]
            [clojuratica.debug]))

(declare string-to-expr build-set-expr add-head convert)

(defmulti express (fn [expression & _] (class expression)))

(defmethod express String [s kernel-link]
  (convert (string-to-expr s kernel-link)))

(defmethod express Expr [expr & [kernel-link]]
  (convert expr))

(defmethod express CExpr [cexpr & [kernel-link]]
  cexpr)

(defmulti send-read (fn [expression & _] (class expression)))

(defmethod send-read String [s kernel-link]
  (io! "The Clojuratica function you have called has side effects."
    (locking kernel-link
      (if debug (println "send-read String>" s))
      (.evaluate kernel-link s)
      (.waitForAnswer kernel-link)
      (let [output (.. kernel-link getExpr)]
        (if debug (println "output>" output))
        (convert output)))))

(defmethod send-read Expr [expr kernel-link]
  (io! "The Clojuratica function you have called has side effects."
    (locking kernel-link
      (if debug (println "send-read Expr>" expr))
      (.evaluate kernel-link expr)
      (.waitForAnswer kernel-link)
      (let [output (.. kernel-link getExpr)]
        (if debug (println "output>" output))
        (convert output)))))

(defmethod send-read CExpr [cexpr kernel-link]
  (send-read (.getExpr cexpr) kernel-link))

(defn convert
  [arg1 & [arg2]]
  (if arg2
    (CExpr. arg1 arg2)
    (CExpr. arg1)))

(defn add-head
  [head exprs]
  (let [loop (com.wolfram.jlink.MathLinkFactory/createLoopbackLink)]
    (.putFunction loop head (count exprs))
    (dorun
      (for [expr exprs] (.put loop expr)))
    (.endPacket loop)
    (.getExpr loop)))

(defnf build-module [[:all-output :last-output]
                     [:parallel :serial]]
                    []
  "Creates an Expr containing a Mathematica module. The syntax of build-module is similar to that
  of Clojure's let. The Mathematica module created will have local (lexically scoped) variables
  as specified in the first argument to build-module, which must be a vector of pairs, as in
  Clojure's let. build-module converts each pair to a Mathematica Set[] expression using build-set-expr.
  The remaining arguments, after the vector, must be expressions. These expressions can be Expr objects,
  CExpr objects, strings, or a combination. Flags may be passed anywhere in the argument list.
  Allowed flags:
  :parallel - Wraps the expressions in a ParallelSubmit[]. This is for use by the functions
              in clojuratica.parallel.
  :serial - Default. Opposite of :parallel.
  :all-output - Packages expressions in a list, such that the output of each expression is returned.
  :last-output - Default. Packages expressions in a CompoundExpression[], such that the output
                 of only the last expression is returned. Opposite of all-output."
  [flags]
  [& args]

  (if-not (vector? (first args))
    (throw (Exception. (str "First non-flag argument to Clojuratica evaluator or module-builder"
                            "must be a vector (possibly empty) of bindings."))))
  (let [set-specs      (first args)
        set-spec-seqs  (partition 2 set-specs)
        set-expr-seq   (for [set-spec-seq set-spec-seqs]
                         (apply build-set-expr set-spec-seq))
        local-vars     (for [set-expr set-expr-seq]
                         (if (> (count (.args set-expr)) 0)
                           (.part set-expr 1)
                           set-expr))
        expression-seq (drop-last (rest args))
        kernel-link    (last args)
        cexpr-seq      (for [expression expression-seq]
                         (express expression kernel-link))
        expr-seq       (map (memfn getExpr) cexpr-seq)
        loop           (com.wolfram.jlink.MathLinkFactory/createLoopbackLink)
        compounder     (if (flags :all-output)
                         "List"
                         "CompoundExpression")]
    (.putFunction loop "Module" 2)
    (.put loop
      (add-head "List" set-expr-seq))
    (when (flags :parallel)
      (.putFunction loop "ParallelSubmit" 2)
      (.put loop
        (add-head "List" local-vars)))
    (.put loop
      (add-head compounder expr-seq))
    (.endPacket loop)
    (.getExpr loop)))

(defn build-set-expr
  "Creates an Expr containing a Mathematica Set[] expression. The righthand side (rhs) of the assignment
  is converted to a CExpr and then an Expr using the convert function."
  [lhs rhs]
  (let [loop (com.wolfram.jlink.MathLinkFactory/createLoopbackLink)
        lhs  (.getExpr (convert (symbol lhs)))]
    (if (= rhs :undefined)
      (do
        (.put loop lhs)
        (.endPacket loop)
        (.getExpr loop))
      (let [rhs (.getExpr (convert rhs))]
        (.putFunction loop "Set" 2)
        (.put loop lhs)
        (.put loop rhs)
        (.endPacket loop)
        (.getExpr loop)))))

(defn string-to-expr
  "Converts a string, s, to a Mathematica expression (i.e. an Expr object). Uses the Mathematica
  kernel on the other end of kernel-link to interpret Mathematica syntax."
  [s kernel-link]
  (io! "The Clojuratica function you have called has side effects."
    (let [held-s (str "HoldComplete[" s "]")]
      (locking kernel-link
        (if debug (println "string-to-expr>" held-s))
        (.evaluate kernel-link held-s)
        (.waitForAnswer kernel-link)
        (let [result (.. kernel-link getExpr args)]
          (if-not (first result)
            (throw (Exception. (str "Invalid expression: " s))))
          (if (next result)
            (throw (Exception. (str "Invalid expression: " s))))
          (first result))))))




