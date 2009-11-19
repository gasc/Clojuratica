(import '[com.wolfram.jlink MathLinkFactory])
(def kernel-link (MathLinkFactory/createKernelLink
          "-linkmode launch -linkname 'c:\\program files\\wolfram research\\mathematica\\7.0\\mathkernel.exe'"))
(.discardAnswer kernel-link)

(use 'clojuratica)

(def *kernel* (clojuratica.base.kernel/kernel kernel-link))
(def *default-options* clojuratica.runtime.default-options/*default-options*)

(def math-eval (math-evaluator kernel-link))
(math-eval '(Needs "HashMaps`"))
(math-eval '(Needs "Parallel`Developer`"))
(math-intern math-eval [math CompoundExpression])
(math-intern math-eval FactorInteger)

(comment
  (math (LaunchKernels))
  (math :parallel (FactorInteger 1000002201000010000100031000010000100003100051) :verbose))

