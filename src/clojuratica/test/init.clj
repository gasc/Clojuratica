(ns clojuratica.test.init
 (:use [clojuratica]
	     [clojuratica.runtime.dynamic-vars]))

(import '[com.wolfram.jlink MathLinkFactory])
(def kernel-link (MathLinkFactory/createKernelLink
          "-linkmode launch -linkname 'c:\\program files\\wolfram research\\mathematica\\7.0\\mathkernel.exe'"))
(.discardAnswer kernel-link)

(use 'clojuratica)

(def kernel (clojuratica.base.kernel/kernel kernel-link))
(def options clojuratica.runtime.default-options/*default-options*)

(def math-eval (math-evaluator kernel-link))
(math-intern math-eval :scopes)
(def-math-macro math math-eval)


