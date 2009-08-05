(ns clojuratica.global-setter
  (:use [clojuratica.lib]
        [clojuratica.core]))

(defn global-set [lhs rhs evaluate]
  (let [result (evaluate [] (build-set-expr lhs rhs))]
    (if (evaluate :parallel?)
      (evaluate [] (str "DistributeDefinitions[" lhs "]")))
    result))
