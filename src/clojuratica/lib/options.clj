(ns clojuratica.lib.options)

(defn filter-params [current-options]
  (into {} (filter #(keyword? (key %)) current-options)))

(defn filter-flags  [current-options]
  (into {} (filter #(set? (first %)) current-options)))

(defn filter-flag-sets
  [flag current-options]
  (filter #(some #{flag} %) (keys (filter-flags current-options))))

(defn flags-into
  "Conjoins each flag in flags into hash-map *flags*, setting each such flag as the value
  of any keys that contain flag as an element."
  [current-options flags]
  (into current-options
    (for [flag        flags
          active-set (filter-flag-sets flag current-options)]
      [active-set flag])))

(defn params-into
  "Conjoins each key-value pair in options into hash-map *options*, conjoining each such pair
  only if options-keys contains that key."
  [current-options params]
  (into current-options
    (filter #(some #{(first %)} (keys (filter-params current-options))) params)))

(defn options-into [current-options params flags]
  (flags-into (params-into current-options params) flags))

(defn flag? [current-options flag]
  (not (nil? (some #{flag} (vals (filter-flags current-options))))))

(defn parse-options [current-options args]
  (let [pairs   (partition 2 1 args)
        flag?   #(seq (filter-flag-sets % current-options))
        param?  #(contains? (filter-params current-options) %)]
    (loop [remain  args
           args    []
           flags   []
           params  []]
      (cond (not (seq remain))                             [flags params args]
            (#(and (flag? %) (param? %)) (first remain))   (recur (drop 2 remain) args (conj flags (first remain)) (conj params (vec (take 2 remain))))
            (flag? (first remain))                         (recur (rest remain) args (conj flags (first remain)) params)
            (param? (first remain))                        (recur (drop 2 remain) args flags (conj params (vec (take 2 remain))))
            'else                                          (recur (rest remain) (conj args (first remain)) flags params)))))

(defmacro scoping-options [scoper [*options* options+args current-options] parsed-args & body]
 `(let [[flags# params# ~parsed-args] (parse-options ~current-options ~options+args)]
    (~scoper [~*options* (options-into ~current-options params# flags#)]
      ~@body)))

(defmacro binding-options [& args]
 `(scoping-options binding ~@args))

(defmacro let-options [& args]
 `(scoping-options let ~@args))

(defmacro fn-scoping-options [scoper [*options* current-options] arg-vector & body]
 `(fn [& options+args#]
    (~scoper [~*options* options+args# ~current-options] [& parsed-args#]
      (apply (fn ~arg-vector ~@body) parsed-args#))))

(defmacro fn-binding-options [& args]
 `(fn-scoping-options binding-options ~@args))

(defmacro fn-let-options [& args]
 `(fn-scoping-options let-options ~@args))

(defmacro defn-scoping-options [fn-scoper name & args]
 `(def ~name (~fn-scoper ~@args)))

(defmacro defn-binding-options [& args]
 `(defn-scoping-options fn-binding-options ~@args))

(defmacro defn-let-options [& args]
 `(defn-scoping-options fn-let-options ~@args))
