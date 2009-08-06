(ns clojuratica.parser
  (:import [clojuratica CExpr])
  (:use [clojuratica.lib]
        [clojuratica.core]))

(declare parse-atom
         parse-to-lazy-seqs
         parse-to-vectors)

(defnf parse-dispatch
  "Dispatches to the appropriate method. Used by the following multimethods: express, send-read."
  [args] []
  (let [expression (first args)]
    (cond (string? expression)                                  :string
          (instance? com.wolfram.jlink.Expr expression)         :expr
          (instance? CExpr expression)                          :cexpr
          (nil? expression)                                     :nil
          true  (throw
                  (Exception. (str "Argument to parser must be string, Expr, CExpr, or nil."))))))
; Parse

(defmulti parse parse-dispatch)

(defmethodf parse :string
  [[s kernel-link] _ passthrough-flags] []
  ; Takes a string and a KernelLink instance. Converts s to a CExpr using kernel-link, then parses the
  ; resulting CExpr into a Clojure object. Returns this Clojure object. For details on how CExprs
  ; are parsed see the documentation for the CExpr class.
  (if-not (instance? com.wolfram.jlink.KernelLink kernel-link)
    (throw (Exception. "When argument to parse is a string, the parser must have been created with a kernel-link argument.")))
  (apply parse (express s kernel-link) passthrough-flags))

(defmethodf parse :expr
  [[expr] _ passthrough-flags] []
  (apply parse (express expr) passthrough-flags))

(defmethodfd parse :cexpr
  [[cexpr _ mmafn] flags]
  (concat (.getFlags cexpr) [:seq :no-mmafn])
  [[:vector :seq] [:mmafn :no-mmafn]]

  (if (and (flags :mmafn) (nil? mmafn))
    (throw (Exception. "Cannot parse functions using mmafn unless parser was created with an mmafn argument.")))
  (if (flags :vector)
    (parse-to-vectors cexpr)
    (parse-to-lazy-seqs cexpr)))

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
                            (CExpr. expr)))
            elements    (rest cexpr)]
        (map parse-recur elements)))))

(defn parse-to-vectors [cexpr]  ; logic courtesy of Meikel Brandmeyer
  (if-not (.listQ (.getExpr cexpr))
    (parse-atom cexpr)
    (loop [elements (rest cexpr)
           v        []
           stack    nil]
      (if-let [elements (seq elements)]
        (let [first-cexpr (CExpr. (first elements))]
          (if-not (.listQ (.getExpr first-cexpr))
            (recur (next elements) (conj v (parse-atom first-cexpr)) stack)
            (recur (rest first-cexpr) [] (conj stack [(next elements) v]))))
        (if (seq stack)
          (let [[elements prior-v] (peek stack)]
            (recur elements (conj prior-v v) (pop stack)))
          v)))))

