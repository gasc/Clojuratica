(ns clojuratica.test.SimpleClass
  (:gen-class
     :methods [#^{:static true} [simpleStaticMethod   [Object] Object]
                                [simpleInstanceMethod [Object] Object]]))

(defn -simpleStaticMethod [obj]
  (map inc obj))

;(defn -simpleInstanceMethod [this obj1 obj2 obj3]
 ; ()
