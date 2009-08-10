(ns clojuratica.lib)

(defn instances? [classes instances]
  (and (== (count classes) (count instances))
       (every? true? (map instance? classes instances))))

(defn remove-flags [args flagset-coll]
  (remove (set (apply concat flagset-coll)) args))

(defn match-flags [args flagset-coll]
  (set (remove nil? (for [flagset flagset-coll] (some (set flagset) args)))))

(defn parse-flags [args+flags flagset-coll defaults]
  (let [all  (concat args+flags defaults)
        fb1  (match-flags all flagset-coll)
        fb2  (filter keyword? (remove-flags all flagset-coll))
        fb3  (remove-flags all flagset-coll)]
    [fb1 fb2 fb3]))

(defn take-last [i coll]
  (drop (- (count coll) i) coll))

(comment
(defmacro defmethodf
  [name dispatch-val v flaggroup-coll & body]
 `(defmethodfd ~name ~dispatch-val ~v [] ~flaggroup-coll ~@body))

(defmacro defnf [name arg1 & r]
  (let [docstring-splicer          (if (string? arg1) (list arg1) '())
        [v flaggroup-coll & body]  (if (string? arg1) r (cons arg1 r))]
   `(defnfd ~name ~@docstring-splicer ~v [] ~flaggroup-coll ~@body)))

(defmacro defmethodfd
  [name dispatch-val [v1 v2 v3 v4] default-flags flaggroup-coll & body]
  (let [all-args                    (or v1 `_#)
        local-flags                 (or v2 `_#)
        passthrough-flags           (or v3 `_#)
        passthrough                 (or v4 `_#)]
   `(defmethod ~name ~dispatch-val [& args#]
      (let [~all-args          (remove keyword? args#)
            ~local-flags       (match-flags (concat args# ~default-flags) ~flaggroup-coll)
            ~passthrough-flags (filter keyword? (remove-flags args# ~flaggroup-coll))
            ~passthrough       (remove-flags args# ~flaggroup-coll)]
        ~@body))))

(defmacro defnfd [name arg1 & r]
  (let [docstring-splicer           (if (string? arg1) (list arg1) '())
       [[v1 v2 v3 v4] default-flags
        flaggroup-coll & body]      (if (string? arg1) r (cons arg1 r))
        all-args                    (or v1 `_#)
        local-flags                 (or v2 `_#)
        passthrough-flags           (or v3 `_#)
        passthrough                 (or v4 `_#)]
   `(defn ~name ~@docstring-splicer [& args#]
      (let [~all-args          (remove keyword? args#)
            ~local-flags       (match-flags (concat args# ~default-flags) ~flaggroup-coll)
            ~passthrough-flags (filter keyword? (remove-flags args# ~flaggroup-coll))
            ~passthrough       (remove-flags args# ~flaggroup-coll)]
        ~@body))))
)

(defmacro defnf-*
[macro name parse-flaggable? dispatcher-list flagset-coll defaults doc?-flagbind? flagbind?-bind? & bind?+body]
  (let [[doc-list [fb1 fb2 fb3] bind body] (if (string? doc?-flagbind?)
                                             [(list doc?-flagbind?) flagbind?-bind? (first bind?+body) (rest bind?+body)]
                                             ['() doc?-flagbind? flagbind?-bind? bind?+body])
        [fb1 fb2 fb3]                      (map #(or % `_#) [fb1 fb2 fb3])]
   `(~macro ~name ~@dispatcher-list ~@doc-list [& args+flags#]
      (let [args#            (remove keyword? args+flags#)
            flagged-args#    (if ~parse-flaggable? (filter (fn [arg#] (instance? clojuratica.Flaggable arg#)) args#) ())
            arg-flags#       (apply concat (map (fn [arg#] (.getFlags arg#)) flagged-args#))]
        (letfn [(fn# ~bind
                  (let [defaults#        (concat arg-flags# ~defaults)
                        [~fb1 ~fb2 ~fb3] (parse-flags args+flags# ~flagset-coll defaults#)]
                    ~@body))]
          (apply fn# args#))))))

(defmacro defnfa [name & remainder]
 `(defnf-* defn ~name true () ~@remainder))

(defmacro defnf [name & remainder]
 `(defnf-* defn ~name false () ~@remainder))

(defmacro defmethodfa [name dispatcher & remainder]
 `(defnf-* defmethod ~name true (~dispatcher) ~@remainder))

(defmacro defmethodf [name dispatcher & remainder]
 `(defnf-* defmethod ~name false (~dispatcher) ~@remainder))

;(defmacro defnf-f [name flagset-coll & remainder]
; `(defnf-fd ~name ~flagset-coll [] ~@remainder))
;
;(defmacro defnf [name & remainder]
; `(defnf-f ~name [] ~@remainder))
