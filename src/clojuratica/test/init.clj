
(use 'clojuratica.clojuratica)
(use 'clojuratica.lib)
(import '[com.wolfram.jlink MathLinkFactory])
(require '[clojuratica.core :as core] '[clojuratica.low-level :as low-level])
(def kernel-link (MathLinkFactory/createKernelLink
          "-linkmode launch -linkname 'c:\\program files\\wolfram research\\mathematica\\7.0\\mathkernel.exe'"))
(.discardAnswer kernel-link)

(def evaluate (get-evaluator kernel-link))
(def mmafn (get-mmafn evaluate))
(def parse (get-parser kernel-link mmafn))
(def math (comp parse evaluate))
(def global-set (get-global-setter evaluate))

(def pevaluate (get-evaluator kernel-link :parallel))
(def pmmafn (get-mmafn pevaluate))
(def pparse (get-parser kernel-link pmmafn))
(def pmath (comp pparse pevaluate))
(def pglobal-set (get-global-setter pevaluate))
