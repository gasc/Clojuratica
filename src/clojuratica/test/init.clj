
(use 'clojuratica.clojuratica)
(use 'clojuratica.lib)
(import '[com.wolfram.jlink MathLinkFactory] '[clojuratica.CLink])
(require '[clojuratica.core :as core] '[clojuratica.low-level :as low-level])
(def kernel-link (MathLinkFactory/createKernelLink
          "-linkmode launch -linkname 'c:\\program files\\wolfram research\\mathematica\\7.0\\mathkernel.exe'"))
(.discardAnswer kernel-link)

(def evaluate (get-evaluator kernel-link))
(def fn-wrap (get-fn-wrapper evaluate))
(def parse (get-parser kernel-link fn-wrap))
(def math (comp parse evaluate))
(def global-set (get-global-setter evaluate))

(def pevaluate (get-evaluator kernel-link :parallel))
(def pfn-wrap (get-fn-wrapper pevaluate))
(def pparse (get-parser kernel-link pfn-wrap))
(def pmath (comp pparse pevaluate))
(def pglobal-set (get-global-setter pevaluate))
