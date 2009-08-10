;(ns clojuratica.Flaggable
;  (:gen-interface
;   :methods [[getFlags [] clojure.lang.IPersistentCollection]]))
(ns clojuratica.Flaggable)
(gen-interface :name clojuratica.Flaggable :methods [[getFlags [] clojure.lang.IPersistentCollection]])
