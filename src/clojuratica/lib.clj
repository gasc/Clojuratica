(ns clojuratica.lib)

(defn instances? [classes instances]
  (and (== (count classes) (count instances))
       (every? true? (map instance? classes instances))))

(defn remove-flags [args flag-sets]
  (remove (set (apply concat flag-sets)) args))

(defn match-flags [args flag-sets]
  (set (for [flag-set flag-sets] (some (set flag-set) args))))

(defn parse-flags [args & [flag-set]]
  (let [all-args              (remove keyword? args)
        local-flags           (match-flags args flag-set)
        passthrough-flags     (set (filter keyword? (remove-flags args flag-set)))
        passthrough-all       (remove-flags args flag-set)]
    [all-args local-flags passthrough-flags passthrough-all]))

(defn take-last [i coll]
  (drop (- (count coll) i) coll))

(defmacro defmethodf [name dispatch-val v flag-set & body]
 `(defmethod ~name ~dispatch-val [& args#]
    (let [~v (parse-flags args# ~flag-set)]
      ~@body)))

(defmacro defnf [name arg1 & remainder]
  (let [docstring           (if (string? arg1) (list arg1) '())
        [v flag-set & body] (if (string? arg1) remainder (cons arg1 remainder))]
   `(defn ~name ~@docstring [& args#]
      (let [~v (parse-flags args# ~flag-set)]
        ~@body))))

