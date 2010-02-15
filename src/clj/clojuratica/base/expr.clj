(ns clojuratica.base.expr
  (:import [com.wolfram.jlink Expr]))

(defn head-str [expr]
  (assert (instance? com.wolfram.jlink.Expr expr))
  (.toString (.head expr)))

(defn parts [expr]
  (assert (instance? com.wolfram.jlink.Expr expr))
  (cons (.head expr) (seq (.args expr))))

(defn expr-from-parts [expr-coll]
  (assert (every? #(instance? com.wolfram.jlink.Expr %) expr-coll))
  (Expr. (first expr-coll) (into-array Expr (rest expr-coll))))


