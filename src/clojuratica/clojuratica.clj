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

(ns clojuratica.clojuratica
  (:require [clojuratica.core]
            [clojuratica.serial]
            [clojuratica.parallel]) 
  (:use [clojuratica.lib]))

(defn get-evaluator
  "Valid flags: :parallel :serial
  Passthrough flags allowed"
  [& args]
  (let [flags (flags args [[:parallel :serial]])
        args  (remove-flags args [[:parallel :serial]])]
    (when-not (instance? com.wolfram.jlink.KernelLink (first args))
      (throw (Exception. "First non-flag argument to get-evaluator must be a KernelLink object.")))
    (if (some #{:parallel} flags)
      (apply clojuratica.parallel/get-evaluator args)
      (apply clojuratica.serial/get-evaluator args))))

(defn get-parser [& [kernel-link]]
  "No valid flags."
  (fn [arg] (clojuratica.core/parse arg kernel-link)))

(defn get-global-setter
  [kernel-link]
  ;(let [flags (flags args [[:parallel :serial]])
        ;args  (remove-flags args [[:parallel :serial]])
        ;kernel-link (first args)]
    ;(if (some #{:parallel} flags)
      (fn [lhs rhs] (clojuratica.parallel/global-set lhs rhs kernel-link)))
      ;(fn [lhs rhs] (clojuratica.serial/global-set lhs rhs kernel-link)))))







