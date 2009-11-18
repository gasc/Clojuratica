(ns clojuratica.base.cep
  (:use [clojuratica.runtime.dynamic-vars]
        [clojuratica.runtime.default-options]
        [clojuratica.lib.options]
        [clojuratica.base.convert]
        [clojuratica.base.evaluate]
        [clojuratica.base.parse]))

(defn cep [expr]
  (if (= expr :get-dynamic-vars)
    {'*kernel* *kernel* '*options* *options*}
    (binding [*options* (if (flag? *options* :restore-defaults) *default-options* *options*)]
      (let [convert  (if (flag? *options* :convert)   convert  identity)
            evaluate (if (flag? *options* :evaluate)  evaluate identity)
            parse    (if (flag? *options* :parse)     parse    identity)]
        ((comp parse evaluate convert) expr)))))
