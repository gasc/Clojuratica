(ns clojuratica.CLink
    (:gen-class
     :methods [#^{:static true} [convert [com.wolfram.jlink.Expr] Object]
               #^{:static true} [parse [Object] com.wolfram.jlink.Expr]
               #^{:static true} [mirrorClasspath [] Object]]
     :state state)
    (:import [com.wolfram.jlink StdLink Expr]
             [clojure.lang.*]
             [java.io StringReader])
    (:use [clojuratica.clojuratica] [clojuratica.low-level]))

(defn -convert [expr]
  (let [parse (get-parser (StdLink/getLink))]
    (parse expr)))

(defn -parse [obj]
  (.getExpr (convert obj)))

(defn -mirrorClasspath []
  (let [classloader (.getClassLoader (StdLink/getLink))
        m-classpath (seq (.getClassPath classloader))
        m-url-classpath (for [path m-classpath] (str "file://" path))
        c-classpath (seq (.getURLs (clojure.lang.RT/baseLoader)))]
    (doseq [url m-url-classpath]
      (when-not (some #{url} c-classpath)
        (add-classpath url)))))
