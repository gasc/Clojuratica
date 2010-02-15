(ns clojuratica.base.convert
  ;(:require [clojure.contrib.str-utils2 :as str-utils])
  (:use [clojuratica.lib.debug]
        [clojuratica.lib.options]
        [clojuratica.base.parse]
        [clojuratica.base.express]
        [clojuratica.base.expr]
        [clojuratica.runtime.dynamic-vars])
  (:import [com.wolfram.jlink Expr MathLinkFactory]))

(declare convert quote-symbols simple-vector? simple-matrix? sequential-to-array)

(declare cexpr-from-postfix-form
         cexpr-from-prefix-form)

(defn- dispatch [obj]
  (cond (and (list? obj)
             (empty? obj))   :null
        (list? obj)          :expr
        (or (vector? obj)
            (seq? obj))      :list
        (ratio? obj)         :rational
        (map? obj)           :hash-map
        (symbol? obj)        :symbol
        (nil? obj)           :null
        'else                nil))

(defmulti convert dispatch)

(defmethod convert nil [obj]
  (.getExpr
    (doto (MathLinkFactory/createLoopbackLink)
      (.put obj)
      (.endPacket))))

(defmethod convert :null [_]
  (convert 'Null))

(defmethod convert :rational [n]
  (convert (list 'Rational (.numerator n) (.denominator n))))

(defmethod convert :hash-map [map]
  (if (flag? *options* :hash-maps)
    (convert (apply list 'HashMap (for [[key value] map] (list 'Rule key value))))
    (convert (seq map))))

(defmethod convert :symbol [sym]
	(let [all-aliases (into (*options* (*options* :alias-list))
										      (*options* :clojure-scope-aliases))]
		(if-let [alias (all-aliases sym)]
			(convert alias)
			(if-let [[_ n] (re-matches #"%(\d*)" (str sym))]
				(let [n (Long/valueOf (if (= "" n) "1" n))]
					(convert (list 'Slot n)))
				;(let [s (str-utils/replace (str sym) #"\|(.*?)\|" #(str "\\\\[" (second %) "]"))]   )
				(let [s (str sym)]
					(if (re-find #"[^a-zA-Z0-9$\/]" s)
						(throw (Exception. "Symbols passed to Mathematica must be alphanumeric (apart from forward slashes and dollar signs)."))
						(Expr. Expr/SYMBOL (apply str (replace {\/ \`} s)))))))))

(defmethod convert :list [coll]
  (cond (simple-matrix? coll)   (do
                                  (if (flag? *options* :verbose) (println "Converting simple matrix..."))
                                  (convert (to-array-2d coll)))
        (simple-vector? coll)   (do
                                  (if (flag? *options* :verbose) (println "Converting simple vector..."))
                                  (convert (to-array coll)))
        'else                   (do
                                  (if (flag? *options* :verbose) (println "Converting complex list..."))
                                  (convert
                                    (to-array
                                      (map #(cond (dispatch %)         (convert %)
                                                  'else                %)
                                           coll))))))

(defmethod convert :expr [cexpr]
  (let [macro (first cexpr)
        arg   (second cexpr)]
    (cond (= 'clojure.core/deref macro)    (convert (cexpr-from-prefix-form arg))
          (= 'clojure.core/meta macro)     (convert (cexpr-from-postfix-form arg))
          (= 'var macro)                   (convert (list 'Function arg))
          (= 'quote macro)                 (express arg)
          'else                            (expr-from-parts (map convert cexpr)))))

(defn- simple-vector? [coll]
  (and (sequential? coll)
       (not-any? dispatch coll)))

(defn- simple-matrix? [coll]
  (and (sequential? coll)
       (every? simple-vector? coll)))

(defn- cexpr-from-postfix-form [cexprs]
  (assert (sequential? cexprs))
  (loop [cexpr     (first cexprs)
         remaining (rest cexprs)]
    (if (seq remaining)
      (recur (list (first remaining) cexpr) (rest remaining))
      cexpr)))

(defn- cexpr-from-prefix-form [cexprs]
  (assert (sequential? cexprs))
  (cexpr-from-postfix-form (reverse cexprs)))

