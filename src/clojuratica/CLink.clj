(ns clojuratica.CLink
  (:gen-class
   :methods [#^{:static true} [getEvaluator         [Object String]               Object]
             #^{:static true} [getParser            [Object Object String]        Object]
             #^{:static true} [getGlobalSetter      [Object String]               Object]
             #^{:static true} [getMmafn             [Object String]               Object]])
  (:import [com.wolfram.jlink StdLink])
  (:use [clojuratica.clojuratica]
        [clojuratica.lib]))

(defn exprs-to-nils [args]
  (map #(if-not (instance? com.wolfram.jlink.Expr %) %) args))

(defn string-to-keywords [flags]
  (let [flags (or flags "")]
    (map keyword
      (map #(second (re-find #":?(.*)" %))
        (remove empty? (re-split #" " flags))))))

(defn -getEvaluator [& args]
  (let [[kernel-link flag-str] (exprs-to-nils args)
        flags                  (string-to-keywords flag-str)]
    (apply get-evaluator kernel-link flags)))

(defn -getParser [& args]
  (let [[kernel-link mmafn flag-str] (exprs-to-nils args)
        flags                        (string-to-keywords flag-str)]
    (apply get-parser kernel-link mmafn flags)))

(defn -getMmafn [& args]
  (let [[evaluate flag-str]    (exprs-to-nils args)
        flags                  (string-to-keywords flag-str)]
    (apply get-mmafn evaluate flags)))

(defn -getGlobalSetter [& args]
  (let [[evaluate flag-str]    (exprs-to-nils args)
        flags                  (string-to-keywords flag-str)]
    (apply get-global-setter evaluate flags)))

