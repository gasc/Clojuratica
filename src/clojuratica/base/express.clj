(ns clojuratica.base.express
  (use [clojuratica.lib.debug]
       [clojuratica.lib.options]
       [clojuratica.runtime.dynamic-vars]))

(defn express [s]
  (let [kernel-link (*kernel* :link)]
    (assert (string? s))
    (assert (instance? com.wolfram.jlink.KernelLink kernel-link))
    (if (flag? *options* :verbose) (println "express string>" s))
    (let [held-s (str "HoldComplete[" s "]")
          output (io!
                   (locking kernel-link
                     (doto kernel-link
                       (.evaluate held-s)
                       (.waitForAnswer))
                     (.. kernel-link getExpr args)))]
      (if (= (count output) 1)
        (first output)
        (throw (Exception. (str "Invalid expression: " s)))))))
