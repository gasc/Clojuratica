(ns clojuratica.mmafn
  (:use [clojuratica.core] [clojuratica.lib])
  (:import [clojuratica CExpr]
           [com.wolfram.jlink Expr]))

(defn- dispatch
  "Dispatches to the appropriate method. Used by the following multimethods: express, send-read,
  and parse."
  [& args]
  (let [args        (remove-flags args)
        assignments (first args)
        expression  (second args)]
    (if-not (vector? assignments)
      (throw (Exception. (str "First argument to mmafn "
                              "must be a vector of assignments"))))
    (cond (string? expression)          :string
          (instance? Expr expression)   :expr
          (instance? CExpr expression)  :cexpr
          (nil? expression)             :nil
          true (throw (Exception. (str "Expression in mmafn must be "
                                       "string, Expr, CExpr, or nil. Passed an object "
                                       "of class " (class expression)))))))

(defmulti mmafn dispatch)

(defmethod mmafn :string [& args]
  (let [flags       (flags args)
        args        (remove-flags args)
        kernel-link (try (nth args 3) (catch Exception _ (throw (Exception. "Too few args."))))
        evaluator   (try (nth args 2) (catch Exception _ (throw (Exception. "Too few args."))))
        s           (try (nth args 1) (catch Exception _ (throw (Exception. "Too few args."))))
        assignments (try (nth args 0) (catch Exception _ (throw (Exception. "Too few args."))))
        expr        (.getExpr (express s kernel-link))]
    (apply mmafn (concat flags (list assignments expr evaluator kernel-link)))))

(defmethod mmafn :cexpr [& args]
  (let [flags       (flags args)
        args        (remove-flags args)
        kernel-link (try (nth args 3) (catch Exception _ (throw (Exception. "Too few args."))))
        evaluator   (try (nth args 2) (catch Exception _ (throw (Exception. "Too few args."))))
        cexpr       (try (nth args 1) (catch Exception _ (throw (Exception. "Too few args."))))
        assignments (try (nth args 0) (catch Exception _ (throw (Exception. "Too few args."))))
        expr        (.getExpr cexpr)]
    (apply mmafn (concat flags (list assignments expr evaluator kernel-link)))))

(defmethod mmafn :expr [& args]
  (let [flag-sets   [[:parse :no-parse]]
        flags       (flags args flag-sets)
        args        (remove-flags args)
        kernel-link (try (nth args 3) (catch Exception _ (throw (Exception. "Too few args."))))
        evaluator   (try (nth args 2) (catch Exception _ (throw (Exception. "Too few args."))))
        expr        (try (nth args 1) (catch Exception _ (throw (Exception. "Too few args."))))
        assignments (try (nth args 0) (catch Exception _ (throw (Exception. "Too few args."))))
        call        (if (some #{:parse} flags)
                        (comp parse evaluator)
                        evaluator)
        head        (.toString (.part expr 0))]
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
          (call assignments fn-call)))
      (fn [& args]
        (let [lhs             (.part expr 1)
              expressed-args  (map (fn [x] (.getExpr (convert x))) args)
              name            (if (zero? (count (.args lhs)))
                                (.toString lhs)
                                (.toString (.head lhs)))
              assignments     (vec (concat assignments [name :undefined]))
              fn-call         (add-head name expressed-args)]
          (call assignments expr fn-call))))))

(defmethod mmafn :nil [& args]
  nil)
