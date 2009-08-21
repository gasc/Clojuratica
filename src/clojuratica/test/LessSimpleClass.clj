(ns clojuratica.test.LessSimpleClass
  (:gen-class
     :methods [[simpleComputation    [Object] Object]
               [threadedComputation  [Object] Object]]
     :state state
     :init init)
  (:import [com.wolfram.jlink MathLinkFactory])
  (:use [clojuratica.clojuratica]))

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
