(ns clojuratica.integration.intern
  (:refer-clojure :exclude [intern read])
  (:use [clojuratica.lib.options]
        [clojuratica.integration.read]))

(defn filter-symbols [coll]
  (filter
    #(try
      (eval (list 'declare (symbol %)))
      (catch Exception _ false))
    coll))

(defn opspec-pairs [math-eval opspec]
  (cond (string? opspec) (map #(vector % %)
                           (map symbol
                             (filter-symbols
                               (math-eval :restore-defaults (list 'Names opspec)))))
        (symbol? opspec) [[opspec opspec]]
        (and (vector? opspec) (= 2 (count opspec))) [opspec]
        'else (throw (Exception. (str "Invalid opspec in math-intern: " opspec)))))

(defn defmacro-op [math-eval-symbol macro-name op]
  (let [math-eval        (eval math-eval-symbol)
        enclosed-options ((math-eval :get-dynamic-vars) '*options*)]
    (eval
     `(defmacro ~macro-name [& args#]
        (let [[flags# params# args#] (parse-options '~enclosed-options args#)
              expr#                  (read (cons (quote ~op) args#))]
          (list 'apply '~math-eval-symbol expr# (list 'apply 'concat flags# params#)))))))

(defn defn-op [math-eval fn-name op]
  (eval `(def ~fn-name (~math-eval :as-function '~op))))

(defn intern [as math-eval-symbol & opspecs]
  (let [math-eval    (eval math-eval-symbol)
        opspec-pairs (apply concat (map #(opspec-pairs math-eval %) opspecs))]
    (if (= as :macro)
      (doall (for [[macro-name op] opspec-pairs] (defmacro-op math-eval-symbol macro-name op)))
      (doall (for [[fn-name op] opspec-pairs] (defn-op math-eval fn-name op))))))
