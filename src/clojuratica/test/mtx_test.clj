(use 'clojure.contrib.duck-streams)

(defn read-table [filename]
  "Reads a file with whitespace-delimited, tabular data."
  (map #(map (fn [#^String s] (if-not (= s "") (read-string s)))
         (. (. #^String % trim) split "\\s+"))
       (read-lines filename)))

(defn sparse-eigensystem [indices vals m n]
  "Given 3 vectors and the matrix dimensions, returns the eigenvalues and eigenvectors using mathematica."
  (math ["vals" vals
         "indices" indices
         "m" m
         "n" n]
    "Eigensystem[SparseArray[indices->N[vals],{m,n}]]"))

(def m (read-table "C:\\Users\\Garth\\Downloads\\matrix"))
(dorun m)

(def rows (map first m))
(def cols (map second m))
(def values (doall (map #(nth % 2) m)))
(def indices (doall (map list rows cols)))

(time (def es (sparse-eigensystem indices values 2856 2856)))

