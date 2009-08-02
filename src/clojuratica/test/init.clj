
(use 'clojuratica.clojuratica)
(require '[clojuratica.low-level :as low-level])
(import '[com.wolfram.jlink MathLinkFactory])
(def kernel-link (MathLinkFactory/createKernelLink
          "-linkmode launch -linkname 'c:\\program files\\wolfram research\\mathematica\\7.0\\mathkernel.exe'"))
;(def kernel-link (MathLinkFactory/createKernelLink
 ;         "-LinkMode Launch -LinkName \"javaw com.wolfram.jlink.util.LinkSnooper\""))

(.discardAnswer kernel-link)

(def evaluate (get-evaluator kernel-link))

(def parse (get-parser kernel-link))

(def math (comp parse evaluate))

(def global-set (get-global-setter kernel-link))

(def pevaluate (get-evaluator kernel-link :parallel))
(def pmath (comp parse pevaluate))
(require '[clojuratica.core :as core])




(def foo (core/mmafn "Foo[x_]:=1" evaluate kernel-link))



