(ns clojuratica.base.evaluate
  (:use [clojuratica.lib.debug]
        [clojuratica.lib.options]
        [clojuratica.base.convert]
        [clojuratica.runtime.default-options]
        [clojuratica.runtime.dynamic-vars]))

(declare process-state queue-run-or-wait)

(defn evaluate [expr]
  (let [kernel-link (*kernel* :link)]
    (assert (instance? com.wolfram.jlink.Expr       expr))
    (assert (instance? com.wolfram.jlink.KernelLink kernel-link))
    (when (flag? *options* :verbose) (println "evaluate expr>" expr))
    (if (flag? *options* :serial)
      (io!
        (locking kernel-link
          (doto kernel-link (.evaluate expr) (.waitForAnswer))
          (.getExpr kernel-link)))
      (binding-options [*options* [:serial] *options*] _
        (let [pid-expr (evaluate (convert '(Unique Clojuratica/Concurrent/process)))]
          (when (flag? *options* :verbose) (println "pid-expr:" pid-expr))
          (evaluate (convert (list '= pid-expr (list 'ParallelSubmit expr))))
          (evaluate (convert '(QueueRun)))
          (loop []
            (let [[state result] (process-state pid-expr)]
              (if (not= :finished state)
                (do
                  (queue-run-or-wait)
                  (recur))
                (do
                  (evaluate (convert (list 'Remove pid-expr)))
                  result)))))))))

(defn process-state [pid-expr]
  (assert (flag? *options* :serial))
  (let [state-expr    (evaluate (convert (list 'ProcessState pid-expr)))
        state-prefix  (first (.toString state-expr))]
    (cond (= \r state-prefix) [:running nil]
          (= \f state-prefix) [:finished (.part state-expr 1)]
          (= \q state-prefix) [:queued nil]
          true
            (throw (Exception. (str "Error! State unrecognized: " state-expr))))))

(defn queue-run-or-wait []
  (assert (flag? *options* :serial))
  (let [lqr-atom (*kernel* :latest-queue-run-time)
        lqr-time @lqr-atom
        nano-pi  (* 1000000 (*options* :poll-interval))
        run-in   (when lqr-time (- (+ lqr-time nano-pi) (System/nanoTime)))]
    (if (or (nil? run-in) (neg? run-in))
      (do
        (when (flag? *options* :verbose) (println "QueueRunning at time" (System/currentTimeMillis)))
        (evaluate (convert '(QueueRun)))
        (swap! lqr-atom (fn [_] (System/nanoTime))))
      (do
        (Thread/sleep (quot run-in 1000000))
        (when (flag? *options* :verbose) (println "Sleeping for" (quot run-in 1000000) "ms"))))))
