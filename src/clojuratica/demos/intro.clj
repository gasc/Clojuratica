(ns clojuratica.demos.intro
  (:use clojuratica))

(import '[com.wolfram.jlink MathLinkFactory])
(def kernel-link
  (MathLinkFactory/createKernelLink
    "-linkmode launch -linkname 'c:\\program files\\wolfram research\\mathematica\\7.0\\mathkernel.exe'"))
(.discardAnswer kernel-link)

(def math-evaluate (math-evaluator kernel-link))
(defmath math math-evaluate)
(math-intern math-evaluate :clojurian-scopes)


