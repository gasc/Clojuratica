(ns clojuratica.test.LessSimpleClass
  (:gen-class
     :methods [#^{:static true} [simpleMethod         [Object] Object]
                                [simpleComputation    [Object] Object]
                                [threadedComputation  [Object] Object]]
     :state state
     :init init)
  (:import [com.wolfram.jlink MathLinkFactory])
  (:use [clojuratica.clojuratica]))

(defn -init [] [[]
  (let [kernel-link (MathLinkFactory/createKernelLink
                      (str "-linkmode launch -linkname 'c:\\program files"
                           "\\wolfram research\\mathematica\\7.0\\"
                           "mathkernel.exe'"))
        evaluate    (get-evaluator kernel-link)
        p-evaluate  (get-evaluator :parallel kernel-link)
        parse       (get-parser kernel-link)
        run         (comp parse evaluate)
        p-run       (comp parse p-evaluate)]
    (.discardAnswer kernel-link)
    {:kernel-link kernel-link
     :evaluate    evaluate
     :p-evaluate  p-evaluate
     :parse       parse
     :run         run
     :p-run       p-run})])

(defn -simpleMethod [lst]
  (map inc lst))

(defn -simpleComputation [this lst]
  (let [run (:run (.state this))]
    (run ["lst" lst]
      "lst * 7")))

(defn -threadedComputation [this lst]
  (let [p-run (:p-run (.state this))]
    (pvalues (p-run [] "4+5")
             (p-run [] "7+6")
             (p-run [] "8+9")
             (p-run ["lst" lst] "lst + 20"))))
