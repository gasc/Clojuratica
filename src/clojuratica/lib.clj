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
 `(defmethodfd ~name ~dispatch-val ~v [] ~flaggroup-coll ~@body))

(defmacro defmethodfd [name dispatch-val [v1 v2 v3 v4] default-flags flaggroup-coll & body]
  (let [all-args                    (or v1 `_#)
        passthrough-args            (or v2 `_#)
        passthrough-flags           (or v3 `_#)
        passthrough                 (or v4 `_#)]
   `(defmethod ~name ~dispatch-val [& args#]
      (let [~all-args          (remove keyword? args#)
            ~passthrough-args  (match-flags (concat args# ~default-flags) ~flaggroup-coll)
            ~passthrough-flags (filter keyword? (remove-flags args# ~flaggroup-coll))
            ~passthrough       (remove-flags args# ~flaggroup-coll)]
        ~@body))))

(defmacro defnf [name arg1 & r]
  (let [docstring-splicer          (if (string? arg1) (list arg1) '())
        [v flaggroup-coll & body]  (if (string? arg1) r (cons arg1 r))]
   `(defnfd ~name ~@docstring-splicer ~v [] ~flaggroup-coll ~@body)))

(defmacro defnfd [name arg1 & r]
  (let [docstring-splicer           (if (string? arg1) (list arg1) '())
       [[v1 v2 v3 v4] default-flags
        flaggroup-coll & body]      (if (string? arg1) r (cons arg1 r))
        all-args                    (or v1 `_#)
        passthrough-args            (or v2 `_#)
        passthrough-flags           (or v3 `_#)
        passthrough                 (or v4 `_#)]
   `(defn ~name ~@docstring-splicer [& args#]
      (let [~all-args          (remove keyword? args#)
            ~passthrough-args  (match-flags (concat args# ~default-flags) ~flaggroup-coll)
            ~passthrough-flags (filter keyword? (remove-flags args# ~flaggroup-coll))
            ~passthrough       (remove-flags args# ~flaggroup-coll)]
        ~@body))))

