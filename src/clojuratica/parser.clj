(ns clojuratica.parser
  (:import [clojuratica CExpr])
  (:use [clojuratica.lib]
        [clojuratica.core]))

(declare parse-atom
         parse-to-lazy-seqs
         parse-to-vectors)

(defnf parse-dispatch [] []
  "Dispatches to the appropriate method. Used by the following multimethods: express, send-read."
  []
  [& args]

  (let [expression (first args)]
    (cond (string? expression)                                  :string
          (instance? com.wolfram.jlink.Expr expression)         :expr
          (instance? CExpr expression)                          :cexpr
          (nil? expression)                                     :nil
          true  (throw
                  (Exception. (str "Argument to parser must be string, Expr, CExpr, or nil."))))))
; Parse

(defmulti parse parse-dispatch)

(defmethodf parse :string [] []
  [_ passthrough-flags]
  [s kernel-link & [mmafn]]
  ; Takes a string and a KernelLink instance. Converts s to a CExpr using kernel-link, then parses the
  ; resulting CExpr into a Clojure object. Returns this Clojure object. For details on how CExprs
  ; are parsed see the documentation for the CExpr class.
  (if-not (instance? com.wolfram.jlink.KernelLink kernel-link)
    (throw (Exception. "When argument to parse is a string, the parser must have been created with a kernel-link argument.")))
  (apply parse (express s kernel-link) kernel-link mmafn passthrough-flags))

(defmethodf parse :expr [] []
  [_ passthrough-flags]
  [expr & [_ mmafn]]
  (apply parse (express expr) nil mmafn passthrough-flags))

(defmethodfa parse :cexpr [[:vectors :seqs]
                           [:mmafn :no-mmafn]] [:seqs]
  [flags]
  [cexpr & [_ mmafn]]
  (if (and (flags :mmafn) (nil? mmafn))
    (throw (Exception. "Cannot parse functions using mmafn unless parser was created with an mmafn argument.")))
  (let [mmafn (if (flags :no-mmafn) nil mmafn)]
    (if (flags :vectors)
      (parse-to-vectors cexpr mmafn)
      (parse-to-lazy-seqs cexpr mmafn))))

(defmethod parse :nil [& args]
  nil)

(defn parse-atom [cexpr mmafn]
  (let [expr (.getExpr cexpr)]
    (cond (.bigIntegerQ expr)                     (.asBigInteger expr)
          (.bigDecimalQ expr)                     (.asBigDecimal expr)
          (.integerQ expr)                        (.asLong expr)
          (.realQ expr)                           (.asDouble expr)
          (.stringQ expr)                         (.asString expr)
          (= "Null" (.toString expr))             nil
          (= "Function" (.toString (.head expr))) (if mmafn (mmafn [] expr) expr)
          true                                    expr)))

(defn parse-to-lazy-seqs [cexpr mmafn]
  (let [expr (.getExpr cexpr)]
    (if-not (.listQ expr)
      (parse-atom cexpr mmafn)
      (let [parse-recur (fn [expr]
                          (parse-to-lazy-seqs (CExpr. expr) mmafn))
            elements    (rest cexpr)]
        (map parse-recur elements)))))

(defn parse-to-vectors [cexpr mmafn]  ; logic courtesy of Meikel Brandmeyer
  (if-not (.listQ (.getExpr cexpr))
    (parse-atom cexpr mmafn)
    (loop [elements (rest cexpr)
           v        []
           stack    nil]
      (if-let [elements (seq elements)]
        (let [first-cexpr (CExpr. (first elements))]
          (if-not (.listQ (.getExpr first-cexpr))
            (recur (next elements) (conj v (parse-atom first-cexpr mmafn)) stack)
            (recur (rest first-cexpr) [] (conj stack [(next elements) v]))))
        (if (seq stack)
          (let [[elements prior-v] (peek stack)]
            (recur elements (conj prior-v v) (pop stack)))
          v)))))

