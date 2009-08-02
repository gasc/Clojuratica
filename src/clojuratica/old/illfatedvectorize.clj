(comment

(defn vectorize [obj]
  (if-not (seq? obj)
    obj
    (vec (map vectorize obj))))

(defn nth* [coll & indices]
  (reduce nth coll indices))

(defn vectorize-deepest-branches [zipper]
  (loop [loc zipper]
    (if (zip/end? loc)
      (zip/root loc)
      (recur
        (zip/next
          (if (zip/branch? loc)
            loc
            (let [sibling-vector (vec (zip/children (zip/prev loc)))]
              (if (some seq? sibling-vector)
                (throw (Exception. "matrix not square")))
              (zip/replace (zip/prev loc) sibling-vector))))))))

(defn vectorize [sequence]
  (if (seq? sequence)
    (let [zipper (clojure.zip/seq-zip sequence)]
      (recur (vectorize-deepest-branches zipper)))
    sequence))

(defn next-po [loc]
  (if (clojure.zip/right loc)
    (loop [r (clojure.zip/right loc)]
      (if (clojure.zip/branch? r)
        (recur (clojure.zip/down r))
        r))
    (or (clojure.zip/up loc)
        [(clojure.zip/node loc) :end])))

(defn init-po [loc]
  (if (clojure.zip/branch? loc)
    (recur (clojure.zip/down loc))
    loc))

(defn vectorize2 [sequence]
  (let [zipper (init-po (clojure.zip/seq-zip sequence))]
    (loop [loc zipper]
      (if (zip/end? loc)
        (zip/root loc)
        (recur
          (next-po
            (if (zip/branch? loc)
              (zip/replace loc (vec (zip/children loc)))
              loc)))))))

(defn -vectorize [s]
  (loop [v []
         remaining s]
   (if (first remaining)
      (if (seq? (first remaining))
        (recur (conj v
               (first remaining))
        (recur (conj v (first remaining))
               (rest remaining)))
      v))))

(defn -vectorize [obj]
  (loop [v []
         locs (list)
         iters (list obj)]
    (let [vnth* (fn vnth* [& indices]
                  (reduce nth v indices))]
      (if-not iter
        v
        (recur (conj v (first (first iters)))
               (list (inc (first locs)) (rest locs))
               (list (next (first iters)) (rest iters)))))))

(defn deep-lazyseq [first levels]
  (if (zero? levels)
    first
    (recur (lazy-seq (list first 1)) (dec levels))))
)
