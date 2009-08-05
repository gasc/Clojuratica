(ns clojuratica.lib)

(defn instances? [classes instances]
  (and (== (count classes) (count instances))
       (every? true? (map instance? classes instances))))

(defn remove-flags [args flaggroup-coll]
  (remove (set (apply concat flaggroup-coll)) args))

(defn match-flags [args flaggroup-coll]
  (set (for [flaggroup flaggroup-coll] (some (set flaggroup) args))))

(defn parse-flags [args flaggroup-coll]
  (let [all-args           (remove keyword? args)
        local-flags        (match-flags args flaggroup-coll)
        passthrough-flags  (filter keyword? (remove-flags args flaggroup-coll))
        passthrough        (remove-flags args flaggroup-coll)]
    [all-args local-flags passthrough-flags passthrough]))

(defn take-last [i coll]
  (drop (- (count coll) i) coll))

(defmacro defmethodf [name dispatch-val v flaggroup-coll & body]
 `(defmethod ~name ~dispatch-val [& args#]
    (let [~v (parse-flags args# ~flaggroup-coll)]
      ~@body)))

(defmacro defnf [name arg1 & r]
  (let [docstring-splicer          (if (string? arg1) (list arg1) '())
        [v flaggroup-coll & body] (if (string? arg1) r (cons arg1 r))]
   `(defn ~name ~@docstring-splicer [& args#]
      (let [~v (parse-flags args# ~flaggroup-coll)]
        ~@body))))

