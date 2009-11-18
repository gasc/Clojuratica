(ns clojuratica.test.Evaluator2
  (:gen-class
   :methods [#^{:static true} [eval  [Object Object Object]   Object]
             #^{:static true} [so    []   Object]]))

(defn -eval [o1 o2 o3]
  (let [s-expr (list o1 o2 o3)]
    (eval s-expr)))

(defn -so []
  (let [foo (agent [])]
    (send-off foo #(do (Thread/sleep 5000) [%2]) 5)
    foo))

