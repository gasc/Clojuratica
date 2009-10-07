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

(ns clojuratica.clojuratica
  (:require [clojuratica.serial-evaluator   :as serial-evaluator]
            [clojuratica.parallel-evaluator :as parallel-evaluator]
            [clojuratica.fn-wrap            :as fn-wrap]
            [clojuratica.parser             :as parser]
            [clojuratica.global-setter      :as global-setter]
            [clojuratica.displayer          :as displayer])
  (:use [clojuratica.lib]))

(def debug false)

(defnf get-evaluator [[:parallel :serial]] []
  [flags _ passthrough]
  [& _]
  (if (flags :parallel)
    (apply parallel-evaluator/get-evaluator passthrough)
    (apply serial-evaluator/get-evaluator passthrough)))

(defnf get-fn-wrapper [] []
  [_ retained-flags]
  [evaluate]
  (when-not (fn? evaluate)
    (throw (Exception. "First non-flag argument to get-fn-wrapper must be a Clojuratica evaluator.")))
  (fn fn-wrapper [& args]
    (apply fn-wrap/fn-wrap (concat args retained-flags [evaluate]))))

(defnf get-parser [] []
  [_ retained-flags]
  [& [kernel-link fn-wrap]]
  (fn parser [& args]
    (apply parser/parse (concat args retained-flags [kernel-link fn-wrap]))))

(defn get-global-setter
  [evaluate]
  (when-not (fn? evaluate)
    (throw (Exception. "First non-flag argument to get-global-setter must be a Clojuratica evaluator.")))
  (fn global-setter [lhs rhs]
    (global-setter/global-set lhs rhs evaluate)))

(defn get-displayer
  [kernel-link]
  (if-not (instance? com.wolfram.jlink.KernelLink kernel-link)
    (throw (Exception. "First non-flag argument to get-displayer must be a KernelLink instance.")))
  (fn displayer [expression w h]
    (displayer/display expression w h kernel-link)))


(comment
  
(import '[clojuratica CExpr])



(defnf sexpr-convert [[:output :no-output]]
                     [:output]
  [flags]
  [s-expr]
  (let [s-expr (if (flags :no-output)
                 (concat '(CompoundExpression) (list s-expr) '(Null))
                 s-expr)]
    (cond (not (seq? s-expr))                        (CExpr. s-expr)
          (= (first s-expr) 'clojure.core/unquote)   (CExpr. (eval (second s-expr)))
          (= (first s-expr) 'clojure.core/quote)     (CExpr. (eval (second s-expr)))
          (= (first s-expr) 'do)                     (sexpr-convert (cons 'CompoundExpression (rest s-expr)))
          true                                       (CExpr. (map sexpr-convert s-expr) true))))

(defmacro mathematica [math & body]
 `(let [first-results# (dorun
                         (map (fn [cexpr#] (math [] cexpr#))
                              (map (fn [s-expr#] (sexpr-convert :no-output s-expr#)) (drop-last '~body))))
        last-result#   (math [] (sexpr-convert :output (last '~body)))]
   last-result#))

(defmacro cexpr-apply [& parts]
  (if (symbol? (first parts))
    (let [head (symbol (apply str (map #(if (= % \/) \` %) (str (first parts)))))
          args (rest parts)]
      `(CExpr. '~head (list ~@args)))
    `((cexpr-apply ~@(first parts)) ~@(rest parts))))



(defmacro expression-convert [expression]
  (cond (and (seq? expression) (= (first expression) 'quote))
          `(eval ~(second expression))
        (symbol? expression)
          `(CExpr. (quote ~expression))
        (seq? expression)
          (cons 'cexpr-apply
                (map #(if (seq? %)
                       (cons 'expression-convert (list %))
                       %)
                     expression))
        true
          (do (println 1) `(CExpr. ~expression))))

(defmacro multiple-expression-convert [& expressions]
  (if (seq expressions)
    `(cons (expression-convert ~(first expressions))
           (multiple-expression-convert ~@(rest expressions)))))

(defmacro with-math [math & body]
  `(apply ~math [] (multiple-expression-convert ~@body)))

(defmacro testio [code]
  `(do
      (defmacro silly# [] ''(~@code))
      (silly#)))


(defmacro with-math [math & body]
 (do
   (defmacro mathmacro [body] `'(~body))
   `(mathmacro ~body)))

(defmacro foo []
  `'((Plus 1 1) (Times 2 (clojure.core/unquote (+ 1 1)))))

)
