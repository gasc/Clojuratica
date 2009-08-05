(ns clojuratica.parser
  (:import [clojuratica CExpr])
  (:use [clojuratica.lib]
        [clojuratica.core]))

(declare parse-atom
         parse-to-lazy-seqs
         parse-to-vectors)

; Parse

(defmulti parse
  "Turns the first argument into a CExpr using the KernelLink instance provided in the
  second argument. First argument can be a string, an Expr, or a CExpr. Second argument
  is required only if the first argument is a string. Otherwise, second argument is optional."
  common-dispatch)

(defmethodf parse :string
  [[s evaluate] _ passthrough-flags] []
  ; Takes a string and a KernelLink instance. Converts s to a CExpr using kernel-link, then parses the
  ; resulting CExpr into a Clojure object. Returns this Clojure object. For details on how CExprs
  ; are parsed see the documentation for the CExpr class.
  (if (nil? evaluate)
    (throw (Exception. "When first argument to parse is a string, second argument must be an evaluator.")))
  (parse (express s (evaluate :get-kernel-link))))

(defmethodf parse :expr
  [[expr] _ passthrough-flags] []
  (parse (express expr)))

(defmethodf parse :cexpr
  [[cexpr] flags] [[:vector :seq]]
  (cond (flags :vector)   (parse-to-vectors cexpr)
        (flags :seq)      (parse-to-lazy-seqs cexpr)
        true              (if (.getVectorFlag cexpr)
                            (parse-to-vectors cexpr)
                            (parse-to-lazy-seqs cexpr)))) ;default

(defmethod parse :nil [& args]
  nil)

(defn parse-atom [cexpr]
  (let [expr (.getExpr cexpr)]
    (cond (.bigIntegerQ expr)            (.asBigInteger expr)
          (.bigDecimalQ expr)            (.asBigDecimal expr)
          (.integerQ expr)               (.asLong expr)
          (.realQ expr)                  (.asDouble expr)
          (.stringQ expr)                (.asString expr)
          (= "Null" (.toString expr))    nil
          true                           expr)))

(defn parse-to-lazy-seqs [cexpr]
  (let [expr (.getExpr cexpr)]
    (if-not (.listQ expr)
      (parse-atom cexpr)
      (let [parse-recur (fn [expr]
                          (parse-to-lazy-seqs
                            (CExpr. expr
                                    0
                                    (.getVectorFlag cexpr))))
            elements    (rest cexpr)]
        (map parse-recur elements)))))

(defn parse-to-vectors [cexpr]  ; logic courtesy of Meikel Brandmeyer
  (if-not (.listQ (.getExpr cexpr))
    (parse-atom cexpr)
    (loop [elements (rest cexpr)
           v        []
           stack    nil]
      (if-let [elements (seq elements)]
        (let [first-cexpr (CExpr. (first elements)
                                         0
                                         (.getVectorFlag cexpr))]
          (if-not (.listQ (.getExpr first-cexpr))
            (recur (next elements) (conj v (parse-atom first-cexpr)) stack)
            (recur (rest first-cexpr) [] (conj stack [(next elements) v]))))
        (if (seq stack)
          (let [[elements prior-v] (peek stack)]
            (recur elements (conj prior-v v) (pop stack)))
          v)))))

