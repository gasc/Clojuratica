(ns clojuratica.CLink
  (:gen-class
   :methods [#^{:static true} [onLoadClass [com.wolfram.jlink.KernelLink] void]
                              [kl [] com.wolfram.jlink.KernelLink]]
   :init init
   :constructors {[] []}
   :state state)
  (:import [com.wolfram.jlink])
  (:require [clojuratica.core] [clojuratica.lib]))

(defn -init []
  [[] (ref nil)])

(defn -onLoadClass [this kernel-link]
  (dosync (ref-set (.state this) kernel-link)))

(defn -kl [this]
  @(.state this))
