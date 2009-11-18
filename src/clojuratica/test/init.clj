
(import '[com.wolfram.jlink MathLinkFactory])
(def kernel-link (MathLinkFactory/createKernelLink
          "-linkmode launch -linkname 'c:\\program files\\wolfram research\\mathematica\\7.0\\mathkernel.exe'"))
(.discardAnswer kernel-link)

(use 'clojuratica)
(use 'clojuratica.base.evaluate)
(use 'clojuratica.base.kernel)
(use 'clojuratica.runtime.dynamic-vars)
(use 'clojuratica.runtime.default-options)
(use 'clojuratica.base.convert)
(use 'clojuratica.lib.options)

(def math-eval (math-evaluator kernel-link))
(math-eval '(Needs "HashMaps`"))
(math-eval '(Needs "Parallel`Developer`"))
(math-intern math-eval [math CompoundExpression])
(math-intern math-eval FactorInteger)

(math (LaunchKernels))
(math :parallel (FactorInteger 1000002201000010000100031000010000100003100051) :verbose)

