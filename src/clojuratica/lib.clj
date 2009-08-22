; ***** BEGIN LICENSE BLOCK *****
; Version: MPL 1.1/GPL 2.0/LGPL 2.1
;
; The contents of this file are subject to the Mozilla Public License Version
; 1.1 (the "License"); you may not use this file except in compliance with
; the License. You may obtain a copy of the License at
; http://www.mozilla.org/MPL/
;
; Software distributed under the License is distributed on an "AS IS" basis,
; WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
; for the specific language governing rights and limitations under the
; License.
;
; The Original Code is the Clojure-Mathematica interface library Clojuratica.
;
; The Initial Developer of the Original Code is Garth Sheldon-Coulson.
; Portions created by the Initial Developer are Copyright (C) 2009
; the Initial Developer. All Rights Reserved.
;
; Contributor(s):
;
; Alternatively, the contents of this file may be used under the terms of
; either the GNU General Public License Version 2 or later (the "GPL"), or
; the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
; in which case the provisions of the GPL or the LGPL are applicable instead
; of those above. If you wish to allow use of your version of this file only
; under the terms of either the GPL or the LGPL, and not to allow others to
; use your version of this file under the terms of the MPL, indicate your
; decision by deleting the provisions above and replace them with the notice
; and other provisions required by the GPL or the LGPL. If you do not delete
; the provisions above, a recipient may use your version of this file under
; the terms of any one of the MPL, the GPL or the LGPL.
;
; ***** END LICENSE BLOCK *****


(ns clojuratica.lib
  (:import [java.util.regex Pattern]))

(defn remove-flags [args flagset-coll]
  (remove (set (apply concat flagset-coll)) args))

(defn match-flags [args flagset-coll]
  (set (remove nil? (for [flagset flagset-coll] (some (set flagset) args)))))

(defn parse-flags [args+flags flagset-coll defaults]
  (let [all  (concat args+flags defaults)
        fb1  (match-flags all flagset-coll)
        fb2  (filter keyword? (remove-flags all flagset-coll))
        fb3  (remove-flags all flagset-coll)]
    [fb1 fb2 fb3]))

(defn take-last [i coll]
  (drop (- (count coll) i) coll))

(defn re-split
  "Splits the string on instances of 'pattern'.  Returns a sequence of
  strings.  Optional 'limit' argument is the maximum number of
  splits.  Like Perl's 'split'."
  ([#^Pattern pattern string] (seq (. pattern (split string))))
  ([#^Pattern pattern string limit] (seq (. pattern (split string limit)))))

(defmacro defnf-*
[macro name parse-flaggable? dispatcher-list flagset-coll defaults doc?-flagbind? flagbind?-bind? & bind?+body]
  (let [[doc-list [fb1 fb2 fb3] bind body] (if (string? doc?-flagbind?)
                                             [(list doc?-flagbind?) flagbind?-bind? (first bind?+body) (rest bind?+body)]
                                             ['() doc?-flagbind? flagbind?-bind? bind?+body])
        [fb1 fb2 fb3]                      (map #(or % `_#) [fb1 fb2 fb3])]
   `(~macro ~name ~@dispatcher-list ~@doc-list [& args+flags#]
      (let [args#            (remove keyword? args+flags#)
            flagged-args#    (if ~parse-flaggable? (filter (fn [arg#] (instance? clojuratica.Flaggable arg#)) args#) ())
            arg-flags#       (apply concat (map (fn [arg#] (.getFlags arg#)) flagged-args#))]
        (letfn [(flag-parser# ~bind
                  (let [defaults#        (concat arg-flags# ~defaults)
                        [~fb1 ~fb2 ~fb3] (parse-flags args+flags# ~flagset-coll defaults#)]
                    ~@body))]
          (apply flag-parser# args#))))))

;(defmacro defnfa [name & remainder]
; `(defnf-* defn ~name true () ~@remainder))

(defmacro defnf [name & remainder]
 `(defnf-* defn ~name false () ~@remainder))

;(defmacro defmethodfa [name dispatcher & remainder]
; `(defnf-* defmethod ~name true (~dispatcher) ~@remainder))

(defmacro defmethodf [name dispatcher & remainder]
 `(defnf-* defmethod ~name false (~dispatcher) ~@remainder))
