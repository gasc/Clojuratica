(ns clojuratica.test.init
 (:use [clojuratica]
	     [clojuratica.runtime.dynamic-vars]))


(import '[com.wolfram.jlink MathLinkFactory])
(def kernel-link (MathLinkFactory/createKernelLink
          "-linkmode launch -linkname '/usr/local/bin/MathKernel'"))
(.discardAnswer kernel-link)

;math -mathlink -linkmode Listen -linkprotocol TCPIP -linkname 65515@127.0.0.1,65516@127.0.0.1
;(import '[com.wolfram.jlink MathLinkFactory])
;(def kernel-link (MathLinkFactory/createKernelLink
;          "-linkmode Connect -linkprotocol TCPIP -linkname 65515@127.0.0.1,65516@127.0.0.1"))
;(.discardAnswer kernel-link)

(use 'clojuratica)

(def kernel (clojuratica.base.kernel/kernel kernel-link))
(def options clojuratica.runtime.default-options/*default-options*)

(def math-eval (math-evaluator kernel-link))
(math-intern math-eval :scopes)
(def-math-macro math math-eval)


