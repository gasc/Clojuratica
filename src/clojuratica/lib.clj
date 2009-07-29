(ns clojuratica.lib)

(defn instances? [classes instances]
  (and (== (count classes) (count instances))
       (every? true? (map instance? classes instances))))

(defn flags [args & [flag-sets]]
  (if flag-sets
    (for [flag-set flag-sets] (some (set flag-set) args))
    (filter keyword? args)))

(defn remove-flags [args & [flag-sets]]
  (if flag-sets
    (remove (set (apply concat flag-sets)) args)
    (remove keyword? args)))
