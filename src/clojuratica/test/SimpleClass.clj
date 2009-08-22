(ns clojuratica.test.SimpleClass
  (:gen-class
     :methods [#^{:static true} [increment          [Object]                 Object]
                                [decrement          [Object]                 Object]
               #^{:static true} [pair               [Object Object]          Object]
               #^{:static true} [myMap              [Object Object]          Object]
               #^{:static true} [mmaIncrement       [Object Object Object]   Object]]))

(defn -increment [obj]
  (inc obj))

(defn -decrement [this obj]
  (dec obj))

(defn -pair [coll1 coll2]
  (partition 2 (interleave coll1 coll2)))

(defn -myMap [f coll]
  (map f coll))

(defn -mmaIncrement [obj evaluate parse]
  (parse (evaluate ["obj" obj] "obj + 1")))
