
(use 'clojuratica.clojuratica)
(use 'clojuratica.lib)
(require '[clojuratica.low-level :as low-level])
(import '[com.wolfram.jlink MathLinkFactory])
(def kernel-link (MathLinkFactory/createKernelLink
          "-linkmode launch -linkname 'c:\\program files\\wolfram research\\mathematica\\7.0\\mathkernel.exe'"))
;(def kernel-link (MathLinkFactory/createKernelLink
 ;         "-LinkMode Launch -LinkName \"javaw com.wolfram.jlink.util.LinkSnooper\""))

(.discardAnswer kernel-link)

(def evaluate (get-evaluator kernel-link))
 
(def parse (get-parser evaluate))

(def math (comp parse evaluate))

(def global-set (get-global-setter evaluate))

(def pevaluate (get-evaluator kernel-link :parallel))

(def pmath (comp parse pevaluate))

(def mmafn (get-mmafn evaluate))

(require '[clojuratica.core :as core] '[clojuratica.low-level :as low-level])

(def foo (mmafn ["foob" -6] "Foo=Function[{x},x]" :parse))

