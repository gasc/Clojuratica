(use 'clojuratica.clojuratica)

(import '[com.wolfram.jlink MathLinkFactory])
(def kernel-link (MathLinkFactory/createKernelLink
          "-linkmode launch -linkname 'c:\\program files\\wolfram research\\mathematica\\7.0\\mathkernel.exe'"))
(.discardAnswer kernel-link)

(def evaluate (get-evaluator kernel-link))
(def parse (get-parser kernel-link))
(def math (comp parse evaluate))

(assert (= 2 (math [] "1+1")))
(assert (= '(1 2 3) (math [] "{1,2,3}" :seqs)))
(assert (= [1 2 3] (math [] "{1,2,3}" :vectors)))
(assert (= [1 2 3] (math ["x" 3] "{1,2,x}" :vectors)))

(def mmafn (get-mmafn kernel-link evaluate))

(def increment-me (mmafn ["one" 1] "Function[{x}, x + one]"))
(assert (= 2 (parse (increment-me 1))))

(def mmafn (get-mmafn kernel-link evaluate :parse))

(def increment-me (mmafn ["one" 1] "Function[{x}, x + one]"))
(assert (= 2 (increment-me 1)))


