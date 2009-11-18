(ns clojuratica.base.kernel)

(defn kernel [kernel-link]
  {:link                  kernel-link
   :latest-queue-run-time (atom nil)})
