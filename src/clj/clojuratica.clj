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

(ns clojuratica
  (:refer-clojure :exclude [intern])
  (:use [clojuratica.lib.options]
        [clojuratica.runtime.dynamic-vars]
        [clojuratica.runtime.default-options]
        [clojuratica.base.cep]
        [clojuratica.base.convert]
        [clojuratica.base.evaluate]
        [clojuratica.base.kernel]
        [clojuratica.integration.intern]))

(defn-let-options math-evaluator [enclosed-options *default-options*] [kernel-link & [init]]
  (let [enclosed-kernel (kernel kernel-link)]
    (binding [*options* enclosed-options
              *kernel*  enclosed-kernel]
			(evaluate (convert init))
			(evaluate (convert '(Needs "Parallel`Developer`")))
			(evaluate (convert '(Needs "Developer`")))
			(evaluate (convert '(ParallelNeeds "Developer`")))
			(evaluate (convert '(Needs "ClojurianScopes`")))
			(evaluate (convert '(ParallelNeeds "ClojurianScopes`")))
			(evaluate (convert '(Needs "HashMaps`")))
			(evaluate (convert '(ParallelNeeds "HashMaps`"))))
    (fn-binding-options [*options* enclosed-options] [expr]
      (binding [*kernel* enclosed-kernel]
        (cep expr)))))

(defmacro math-intern [& args] 
  (let-options [options args {#{:as-function :as-macro} :as-macro
															#{:no-scopes :scopes}     :no-scopes}]
							 [math-eval & opspecs]
		(let [opspecs (if (flag? options :scopes)
									  (into opspecs (keys (*default-options* :clojure-scope-aliases)))
										opspecs)]
			(if (flag? options :as-macro)
				`(intern :macro '~math-eval ~@(map (fn [opspec#] (list 'quote opspec#)) opspecs))
				`(intern :fn    '~math-eval ~@(map (fn [opspec#] (list 'quote opspec#)) opspecs))))))

(defmacro def-math-macro [m math-eval]
  `(math-intern :as-macro ~math-eval [~m ~'CompoundExpression]))

(defn mathematica->clojure [s math-eval]
  (math-eval :no-evaluate (list 'quote s))) 
