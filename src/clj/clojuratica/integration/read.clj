(ns clojuratica.integration.read
  (:refer-clojure :exclude [read]))

(defn read [expr]
  (cond (symbol? expr) (list 'quote expr)
        (and (seq? expr) (= 'clojure.core/unquote (first expr)))
                       (second expr)
        (and (seq? expr) (= 'clojure.core/unquote-splicing (first expr)))
                       (read (cons 'Sequence (second expr)))
        (seq? expr)    (cons 'list (map read expr))
        (vector? expr) (vec (map read expr))
        (map? expr)    (zipmap (map read (keys expr))
                         (map read (vals expr)))
        'else          expr))
