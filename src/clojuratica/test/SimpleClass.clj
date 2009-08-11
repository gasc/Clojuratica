(ns clojuratica.test.SimpleClass
  (:gen-class
     :methods [#^{:static true} [increment   [Object]        Object]
               #^{:static true} [clojureMap  [Object Object] Object]]))

(defn -increment [obj]
  (map inc obj))

(defn -clojureMap [obj1 obj2]
  (map obj1 obj2))

;(defn -simpleInstanceMethod [this obj1 obj2 obj3]
 ; ()
