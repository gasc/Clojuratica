(ns clojuratica.test.SimpleClass
  (:gen-class
     :methods [#^{:static true} [increment   [Object]                             Object]
               #^{:static true} [mIncrement  [Object Object Object Object]        Object]
               #^{:static true} [clojureMap  [Object Object]                      Object]])
  (:import [com.wolfram.jlink StdLink])
  (:use [clojuratica.clojuratica]))

(defn -increment [obj]
  (map inc obj))

(defn -mIncrement [obj evaluate parse mmafn]
  (parse (evaluate ["obj" obj] "Map[#+1&, obj]" "10+obj")))

(defn -clojureMap [obj1 obj2]
  (map obj1 obj2))

;(defn -simpleInstanceMethod [this obj1 obj2 obj3]
 ; ()
