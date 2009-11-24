(ns clojuratica.base.parse
  (:import [com.wolfram.jlink Expr])
  (:use ;[clojure.par]
        [clojuratica.lib.debug]
        [clojuratica.lib.options]
        [clojuratica.base.expr]
        [clojuratica.runtime.dynamic-vars]))

(in-ns 'clojuratica.base.cep)
(declare cep)
(in-ns 'clojuratica.base.parse)
(refer 'clojuratica.base.cep)

(declare
	parse
	simple?
	atom?
	simple-array-type
	simple-vector-type
	simple-matrix-type
	bound-map
	vec-bound-map
	seq-bound-map
	seq-fn-bound-map
	parse-simple-vector
	parse-simple-matrix
	parse-simple-atom
	parse-complex-atom
	parse-complex-list
	parse-integer
	parse-rational
	parse-symbol
	parse-hash-map
	parse-fn
	parse-generic-expression)

(defn parse [expr]
  (assert (instance? com.wolfram.jlink.Expr expr))
  (cond (flag? *options* :as-function)                   (parse-fn expr)
        (or (atom? expr) (flag? *options* :full-form))   (parse-complex-atom expr)
        (simple-vector-type expr)                        (parse-simple-vector expr)
        (simple-matrix-type expr)                        (parse-simple-matrix expr)
        'else                                            (parse-complex-list expr)))

(defn simple? [expr]
  (or (atom? expr) (simple-array-type expr)))

(defn atom? [expr]
  (not (.listQ expr)))

(defn simple-array-type [expr]
  (or (simple-vector-type expr) (simple-matrix-type expr)))

(defn simple-vector-type [expr]
  (cond (.vectorQ expr Expr/INTEGER)     Expr/INTEGER
        (.vectorQ expr Expr/BIGINTEGER)  Expr/BIGINTEGER
        (.vectorQ expr Expr/REAL)        Expr/REAL
        (.vectorQ expr Expr/BIGDECIMAL)  Expr/BIGDECIMAL
        (.vectorQ expr Expr/STRING)      Expr/STRING
        (.vectorQ expr Expr/RATIONAL)    Expr/RATIONAL
        (.vectorQ expr Expr/SYMBOL)      Expr/SYMBOL
        'else                            nil))

(defn simple-matrix-type [expr]
  (cond (.matrixQ expr Expr/INTEGER)     Expr/INTEGER
        (.matrixQ expr Expr/BIGINTEGER)  Expr/BIGINTEGER
        (.matrixQ expr Expr/REAL)        Expr/REAL
        (.matrixQ expr Expr/BIGDECIMAL)  Expr/BIGDECIMAL
        (.matrixQ expr Expr/STRING)      Expr/STRING
        (.matrixQ expr Expr/RATIONAL)    Expr/RATIONAL
        (.matrixQ expr Expr/SYMBOL)      Expr/SYMBOL
        'else                            nil))

(defn vec-bound-map [f coll]
	(vec (map f coll)))

(defn seq-bound-map [f coll]
	(let [kernel  *kernel*
				options *options*]
		(lazy-seq
			(binding [*kernel*  kernel
								*options* options]
				(when-let [s (seq coll)]
					(cons (f (first s))
								(seq-bound-map f (rest s))))))))

(defn seq-fn-bound-map [f coll]
	(let [enclosed-kernel *kernel*]
		(let-options [enclosed-options [:seqs] *options*] []
			(fn-binding-options [*options* enclosed-options] []
				(binding [*kernel* enclosed-kernel]
					(bound-map f coll))))))

(defn bound-map [f coll]
	(cond
		(flag? *options* :vectors)	(vec-bound-map f coll)
		(flag? *options* :seqs)			(seq-bound-map f coll)
		(flag? *options* :seq-fn)		(seq-fn-bound-map f coll)))

(defn parse-simple-vector [expr & [type]]
  (with-debug-message (and (flag? *options* :verbose) (nil? type)) "simple vector parse"
    (let [type (or type (simple-vector-type expr))]
			(if (and (flag? *options* :N)
							 (some #{Expr/INTEGER Expr/BIGINTEGER Expr/REAL Expr/BIGDECIMAL} #{type}))
				((if (flag? *options* :vectors) vec seq) (.asArray expr Expr/REAL 1))
				(bound-map #(parse-simple-atom % type) (.args expr))))))

(defn parse-simple-matrix [expr & [type]]
  (with-debug-message (flag? *options* :verbose) "simple matrix parse"
    (let [type (or type (simple-matrix-type expr))]
      (bound-map #(parse-simple-vector % type) (.args expr)))))

(defn parse-simple-atom [expr type]
	(cond (= type Expr/BIGINTEGER)   (.asBigInteger expr)
				(= type Expr/BIGDECIMAL)   (.asBigDecimal expr)
				(= type Expr/INTEGER)      (parse-integer expr)
				(= type Expr/REAL)         (.asDouble expr)
				(= type Expr/STRING)       (.asString expr)
				(= type Expr/RATIONAL)     (parse-rational expr)
				(= type Expr/SYMBOL)       (parse-symbol expr)))

(defn parse-complex-atom [expr]
  (let [head (head-str expr)]
    (cond (.bigIntegerQ expr)      (.asBigInteger expr)
          (.bigDecimalQ expr)      (.asBigDecimal expr)
          (.integerQ expr)         (parse-integer expr)
          (.realQ expr)            (.asDouble expr)
          (.stringQ expr)          (.asString expr)
          (.rationalQ expr)        (parse-rational expr)
          (.symbolQ expr)          (parse-symbol expr)
          (= "Function" head)      (if (and (flag? *options* :functions) (not (flag? *options* :full-form)))
                                     (parse-fn expr)
                                     (parse-generic-expression expr))
          (= "HashMapObject" head) (if (and (flag? *options* :hash-maps) (not (flag? *options* :full-form)))
                                     (parse-hash-map expr)
                                     (parse-generic-expression expr))
          'else                    (parse-generic-expression expr))))

(defn parse-complex-list [expr]
  (bound-map parse (.args expr)))

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
  (let [aliases (into {} (map (comp vec rseq) (*options* (*options* :alias-list))))
				s       (.toString expr)
				sym     (symbol (apply str (replace {\` \/} s)))]
		(if-let [alias (aliases sym)]
		  alias
			(cond (= "True" s)   true
						(= "False" s)  false
						(= "Null" s)   nil
						'else          sym))))

(defn parse-hash-map [expr]
  (with-debug-message (flag? *options* :verbose) "hash-map parse"
    (let [inside    (first (.args expr))
          rules     (parse
                      (cond (.listQ inside) inside
                            (= "Dispatch" (head-str inside)) (first (.args inside))
                            'else (assert (or (.listQ inside)
                                          (= "Dispatch" (head-str inside))))))
          keys      (map second rules)
          vals      (map (comp second third) rules)]
      (zipmap keys vals))))

(defn parse-fn [expr]
  (with-debug-message (flag? *options* :verbose) "function parse"
    (let [enclosed-kernel *kernel*]
      (let-options [enclosed-options [:as-expression] *options*] []
        (fn-binding-options [*options* enclosed-options] [& args]
          (binding [*kernel* enclosed-kernel]
            (cep (apply list expr args))))))))

(defn parse-generic-expression [expr]
  (-> (list)  ;must start with a real list because the promise is that expressions will be converted to lists
      (into (map parse (rseq (vec (.args expr)))))
      (conj (parse (.head expr)))))

