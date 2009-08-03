
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

(def p-evaluate (get-evaluator kernel-link :parallel))

(def p-math (comp parse p-evaluate))

(def mmafn (get-mmafn p-evaluate kernel-link))

(require '[clojuratica.core :as core])


(def foo (mmafn ["foob" -6] "Foo=Function[{x},x]" :parse))


