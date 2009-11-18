(ns clojuratica.lib.debug)

(defmacro with-debug-message [bool msg & body]
 `(if-not ~bool
    (do ~@body)
    (do
      (println "Starting" (str ~msg "..."))
      (let [result# (do ~@body)]
        (println "Done" (str ~msg "."))
        result#))))

(defn third [coll] (nth coll 2))
