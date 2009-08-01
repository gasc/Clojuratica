(ns clojuratica.CLink
    (:gen-class
     :methods [#^{:static true} [evaluate [String] com.wolfram.jlink.Expr]
               #^{:static true} [parse [com.wolfram.jlink.Expr] Object]
               #^{:static true} [convert [Object] com.wolfram.jlink.Expr]
               #^{:static true} [mirrorClasspath [] Object]
               #^{:static true} [def [String Object] Object]]
     :state state)
    (:import [com.wolfram.jlink StdLink Expr]
             [clojure.lang.*]
             [java.io StringReader])
    (:use [clojuratica.clojuratica] [clojuratica.low-level]))

(defn -parse [expr]
  (let [parse (get-parser (StdLink/getLink))]
    (parse expr)))

(defn -convert [obj]
  (.getExpr (convert obj)))

(defn -evaluate [s]
  (.getExpr (convert (clojure.lang.Compiler/load (StringReader. s)))))

(defn -mirrorClasspath []
  (let [classloader (.getClassLoader (StdLink/getLink))
        m-classpath (seq (.getClassPath classloader))
        m-url-classpath (for [path m-classpath] (str "file://" path))
        c-classpath (seq (.getURLs (clojure.lang.RT/baseLoader)))]
    (doseq [url m-url-classpath]
      (when-not (some #{url} c-classpath)
        (add-classpath url)))))

(defn -def [var value]
  (let [value value])
    (-evaluate (str "(def " var " value)")))
