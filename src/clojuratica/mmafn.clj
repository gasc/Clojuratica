(ns clojuratica.mmafn
  (:use [clojuratica.core]
        [clojuratica.clojuratica]
        [clojuratica.parser]
        [clojuratica.lib])
  (:import [clojuratica CExpr]
           [com.wolfram.jlink Expr]))

(defnf mmafn-dispatch [] []
  []
  [& args]
  (let [assignments  (first args)
        expression   (second args)]
    (if-not (vector? assignments)
      (throw (Exception. (str "First argument to mmafn "
                              "must be a vector of assignments"))))
    (cond (string? expression)          :string
          (instance? Expr expression)   :expr
          (instance? CExpr expression)  :cexpr
          true (throw (Exception. (str "Second argument to mmafn must be "
                                       "string, Expr, or CExpr. You passed an object "
                                       "of class " (class expression)))))))

(defmulti mmafn mmafn-dispatch)

(defmethodf mmafn :string [] []
  [_ passthrough-flags]
  [assignments s kernel-link evaluate]
  (let [expr (.getExpr (express s kernel-link))]
    (apply mmafn assignments expr kernel-link evaluate passthrough-flags)))

(defmethodf mmafn :cexpr [] []
  [_ passthrough-flags]
  [assignments cexpr kernel-link evaluate]
  (let [expr (.getExpr cexpr)]
    (apply mmafn assignments expr kernel-link evaluate passthrough-flags)))

(defmethodf mmafn :expr [[:parse :no-parse]] [:no-parse]
  [flags passthrough-flags]
  [assignments expr kernel-link evaluate]
  (let [head     (.toString (.part expr 0))
        parse    (if (flags :parse)
                   (get-parser kernel-link
                               (apply get-mmafn kernel-link evaluate flags))
                   identity)
        math     (comp parse evaluate)]
    (if-not (or (= "Set"        head)
                (= "SetDelayed" head)
                (= "Function"   head)
                (= "Symbol"     head))
      (throw (Exception. (str "mmafn must be passed a "
                              "string that contains a pure function "
                              "(head Function), a function definition "
                              "(head Set (=) or SetDelayed (:=)), or "
                              "a symbol (head Symbol)."))))
    (if (or (= "Function" head) (= "Symbol" head))
      (fn [& args]
        (let [expressed-args     (map (fn [x] (.getExpr (convert x))) args)
              expressed-arg-list (add-head "List" expressed-args)
              fn-call            (add-head "Apply" [expr expressed-arg-list])]
          (apply math assignments fn-call passthrough-flags)))
      (fn [& args]
        (let [lhs             (.part expr 1)
              expressed-args  (map (fn [x] (.getExpr (convert x))) args)
              name            (if (zero? (count (.args lhs)))
                                (.toString lhs)
                                (.toString (.head lhs)))
              assignments     (vec (concat assignments [name :undefined]))
              fn-call         (add-head name expressed-args)]
          (apply math assignments expr fn-call passthrough-flags))))))
