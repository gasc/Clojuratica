(ns clojuratica.test.vectors)


(def foo
  (clojure.core/fn ([& args+flags__37__auto__]
    (clojure.core/let [args__38__auto__ (if false (clojure.core/remove clojure.core/keyword? args+flags__37__auto__) ())
                       flagged-args__39__auto__ (clojure.core/filter (clojure.core/fn [arg__40__auto__] (clojure.core/instance? clojuratica.Flaggable arg__40__auto__)) args__38__auto__)
                       arg-flags__41__auto__ (clojure.core/apply clojure.core/concat (clojure.core/map (clojure.core/fn [arg__40__auto__] (.getFlags arg__40__auto__)) flagged-args__39__auto__))]
      (clojure.core/letfn [(foo [& args]
        (clojure.core/let [defaults__42__auto__ (clojure.core/concat arg-flags__41__auto__ [])
                           [_ flags _] (clojuratica.lib/parse-flags args+flags__37__auto__ [] defaults__42__auto__)]
          args))]
        (clojure.core/apply foo args__38__auto__))))))

(. foo clojure.core/addMethod :silly
  (clojure.core/fn [& args+flags__37__auto__]
    (clojure.core/let [args__38__auto__ (clojure.core/remove clojure.core/keyword? args+flags__37__auto__)
                       flagged-args__39__auto__ (if false (clojure.core/filter (clojure.core/fn [arg__40__auto__] (clojure.core/instance? clojuratica.Flaggable arg__40__auto__)) args__38__auto__) ())
                       arg-flags__41__auto__ (clojure.core/apply clojure.core/concat (clojure.core/map (clojure.core/fn [arg__40__auto__] (.getFlags arg__40__auto__)) flagged-args__39__auto__))]
      (clojure.core/letfn [(fn__42__auto__ [& args]
                            (clojure.core/let [defaults__43__auto__ (clojure.core/concat arg-flags__41__auto__ [])
                                               [a b c] (clojuratica.lib/parse-flags args+flags__37__auto__ [] defaults__43__auto__)]
                              args))]
        (clojure.core/apply fn__42__auto__ args__38__auto__)))))
