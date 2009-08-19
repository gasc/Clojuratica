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

(ns clojuratica.parallel-evaluator
  (:import [clojuratica CExpr]
           [com.wolfram.jlink Expr])
  (:use [clojuratica.core]
        [clojuratica.parser]
        [clojuratica.lib]))

(declare waitloop manage-queue evaluate)

(defnf get-evaluator [] []
  "No valid flags; passthrough flags allowed"
  [_ retained-flags]
  [kernel-link & [poll-interval]]
  (let [poll-interval       (or poll-interval 15)
        process-queue       (ref {})
        waitloop            #(waitloop kernel-link
                                       process-queue
                                       poll-interval)
        waitloop-thread     (Thread. waitloop)]

    (when-not (instance? com.wolfram.jlink.KernelLink kernel-link)
      (throw (Exception. "First non-flag argument to get-evaluator must be a KernelLink object.")))

    (send-read "Needs[\"Parallel`Developer`\"]" kernel-link)
    (send-read "QueueRun[]" kernel-link)

    (.setDaemon waitloop-thread true)
    (.start waitloop-thread)

    ; This is the anonymous function returned from a call to get-evaluator
    (fn [& args]
      (cond
        (some #{:get-kernel-link} args)
          kernel-link
        (some #{:parallel?} args)
          true
        true
          (apply evaluate
                 (concat args retained-flags [kernel-link
                                              process-queue
                                              waitloop-thread]))))))

(defnf evaluate [] []
  [flags passthrough-flags]
  [& args]
  (let [[kernel-link
         process-queue
         waitloop-thread]      (take-last 3 args)
         passthrough-args      (drop-last 2 args)
         module                (apply build-module :parallel (concat passthrough-flags passthrough-args))
         pid                   (.getExpr (send-read "Unique[Clojuratica`Concurrent`process]" kernel-link))
         pid-string            (.toString pid)
         queue-item            {pid (ref {:thread (Thread/currentThread)})}]

      (send-read (build-set-expr pid-string module) kernel-link)

      (dosync (commute process-queue conj queue-item))

      (.interrupt waitloop-thread)

      (try
        (while true (Thread/sleep 10000))
        (catch InterruptedException _))

      (let [output (CExpr. (:output @(get @process-queue pid)))]
        (dosync (commute process-queue dissoc pid))
        (CExpr. output passthrough-flags))))

(defn waitloop
  [kernel-link process-queue poll-interval]
  ;(println "Waitloop starting...")
  (while true
    (while (seq @process-queue)
        ;(println "Waitloop moving the queue...")
        (send-read "QueueRun[]" kernel-link)
        (manage-queue process-queue kernel-link)
        (try
          (Thread/sleep poll-interval)
          (catch InterruptedException _)))
    (try
      ;(println "Waitloop going to sleep...")
      (while true (Thread/sleep 10000)) ;nothing in list
      (catch InterruptedException _
        ;(println "Waitloop waking up...")
        )))
  ;(println "Waitloop dying...")
  )

(defn manage-queue
  [process-queue kernel-link]
  (let [update-state      (fn [[pid process-data]]
                            (when-not (= :finished (:state @process-data))
                              (let [state-expr        (.getExpr (send-read (add-head "ProcessState" (list pid)) kernel-link))
                                    state-prefix      (apply str (take 3 (.toString state-expr)))
                                    state             (cond (= "run" state-prefix) :running
                                                            (= "fin" state-prefix) :finished
                                                            (= "que" state-prefix) :queued
                                                            true                   (throw (Exception. (str "Error! State unrecognized: " state-expr))))
                                    new-process-data  (conj @process-data {:state state})
                                    new-process-data  (if (= state :finished)
                                                        (conj new-process-data {:output (.part state-expr 1)})
                                                        new-process-data)]
                                (dosync (alter process-data conj new-process-data))))
                            [pid process-data])
      return-if-finished  (fn [[pid process-data]]
                            (when (and (= :finished (:state @process-data))
                                       (not (:returned @process-data)))
                              (let [new-process-data (conj @process-data {:returned true})]
                                (dosync (alter process-data conj new-process-data)))
                              (send-read (add-head "Remove" (list pid)) kernel-link)
                              (.interrupt (:thread @process-data)))
                            [pid process-data])
      update+return       (comp return-if-finished update-state)]
  (dorun (map update+return @process-queue))))

