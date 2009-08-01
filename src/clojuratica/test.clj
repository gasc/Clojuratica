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

; Clojuratica is a Clojure library. It allows easy access to Mathematica from Clojure. The simplest
; paradigm for using Clojuratica is to write the program's control-flow in Clojure and make calls
; to Mathematica when necessary. This tutorial follows that paradigm.

; The reverse paradigm is also possible. To evaluate Clojure code from Mathematica,
; simply create a Java class using the standard Clojure gen-class function and create instances of it
; according to the Mathematica J/Link documentation (or call its static methods).
; A Clojure class called in this way could use Clojuratica to convert
; data structures (aka expressions) passed from Mathematica,
; or to make follow-on calls to Mathematica.

;;;;; Basic set-up

; To begin with, let's make sure clojuratica.jar is in the classpath and use the package.

(use 'clojuratica.clojuratica)

; Clojuratica needs to have access to a Mathematica kernel running locally or remotely. The simplest
; way to provide this access is to launch a local kernel from within Java. The procedure will vary slightly by
; platform and is dictated by the Mathematica J/Link package. On Windows, the following commands
; are appropriate. See the Mathematica J/Link guide for more details. Note that these commands have nothing to
; do with Clojuratica per se. They are general to any Java program that accesses Mathematica.

(import '[com.wolfram.jlink MathLinkFactory])
(def kernel-link (MathLinkFactory/createKernelLink
          "-linkmode launch -linkname 'c:\\program files\\wolfram research\\mathematica\\7.0\\mathkernel.exe'"))
(.discardAnswer kernel-link)

(comment
(import '[com.wolfram.jlink MathLinkFactory])
(def kernel-link (MathLinkFactory/createKernelLink
          "javaw com.wolfram.jlink.LinkSnooper -linkmode launch -linkname 'c:\\program files\\wolfram research\\mathematica\\7.0\\mathkernel.exe'"))
(.discardAnswer kernel-link)

(def ls (LinkSnooper. (into-array (list "-linkmode" "launch" "-linkname" "c:\\program files\\wolfram research\\mathematica\\7.0\\mathkernel.exe"))))
)
; Assuming the above code did not throw a MathLinkException, we now have an active kernel.
; Note that only one master kernel can be active at once under most Mathematica licenses (master
; kernels are difference from parallel kernels, discussed later). A common cause of a MathLinkException
; arising from the code above is trying to launch a kernel while one is already running.

;;;;; The evaluator

; The basic Clojuratica function is the *evaluator*. Getting an evaluator is easy.

(def evaluate (get-evaluator kernel-link))  ;get an evalutor that knows where the Mathematica kernel is

; Now we can do some math.

(evaluate [] "1 + 1")    ; Evaluate the Mathematica expression 1+1.
;(#<Expr Integer>)

; The evaluator's syntax is similar to that of Clojure's let. The first argument is a vector of
; assignments, empty here. The remaining arguments are expressions, in Mathematica's syntax,
; to be evaluated.

; The output of the previous command is a Java object of class CExpr. CExpr stands for Clojuratica
; expression. It is a wrapper class for Mathematica expressions. It can hold
; any Mathematica expession, from a single integer to a sequence of operations on a
; hundred-dimensional array. Remember that Mathematica expressions, and therefore CExprs, are
; homoiconic: they can house
; "pure data" (integers, vectors, matrices, etc.), symbols for symbolic math (x, y, etc.), and
; unevaluated mathematical expressions ("1+1", "x+y", "FactorInteger[15]", etc.).

(class (evaluate [] "1 + 1"))
;clojuratica.CExpr

;;; The parser

; We can easily convert the CExprs we get as evaluator output into Clojure data structures.
; To do this we'll need a Clojuratica *parser*.

(def parse (get-parser kernel-link))    ; Get a Clojuratica parser

(parse (evaluate [] "1 + 1"))    ; Parse the result of evaluating 1+1.
;2

(parse (evaluate [] "3x + 7 + 2x - (10/2)x"))    ;Symbolic math works too
;7

; Parse and evaluate are often used together. It's useful to compose them.

(def math (comp parse evaluate))

(math [] "3x + 7 + 2x - (10/2)x")    ;Pretty syntax
;7

; The evaluator accepts multiple expressions

(math [] 
  "foo = 3x + 7 + 2x - (10/2)x"
  "foobar = foo + 3"
  "foobar ^ 2")
;100

;;;;;; Lists/matrices/arrays as output

; The parser knows how to handle Mathematica lists and arrays. They are converted to
; Clojure data structures on the fly.

(math [] "{{1, 0},
           {0, 1}} * 5")   ; Parse the result of multiplying the 2x2 identity matrix by 5.
;((5 0) (0 5))

; The parser converts Mathematica lists to lazy seqs. Two-dimensional arrays (aka matrices)
; are converted to lazy seqs of lazy seqs. And so on for higher-dimensional arrays.

(class
  (math [] "{{1, 0},
             {0, 1}} * 5"))
;clojure.lang.LazySeq

; Clojuratica has no problem parsing higher-dimensional Mathematica arrays. The following Mathematica
; code creates an eight-dimensional array, two elements in each direction, populated with ones.

(math [] "ConstantArray[1, {2, 2, 2, 2, 2, 2, 2, 2}]")
;((((((((1 1) (1 1)) ((1 1) (1 1))) (((1 1) (1 1)) ((1 1) (1 1 ... truncated ...

;;;;;; Clojuratica is lazy, and that's a good thing

; When parsing Mathematica lists and arrays, the seq returned by the
; parser is lazy. This is helpful for handling large data structures. Only the requested
; elements will be translated into Java/Clojure objects.

; For instance, the following
; Mathematica expression creates a vector with 100,000 elements:

(def long-vector (math [] "Table[i^2, {i, 100000}]"))

; Taking the first 10 elements is fast:

(time (dorun (take 10 long-vector)))
;"Elapsed time: 3.679797 msecs"

; Whereas running through all the elements would take time:

(time (dorun long-vector))
;"Elapsed time: 4976.361032 msecs"

;;;;;; Assignments: Sending data to Mathematica

; Recall that the evaluator permits assignments in its vector first argument. That
; means we can write:

(math ["foo" 2] "foo + 10")  ; Evaluate and parse 2+10.
;12

; The assigned objects can be Clojure data structures. They are automatically converted to
; Mathematica data structures where appropriate.

(math ["foobar" [[1 0]
                 [0 1]]]
  "foobar * 2")
;((2 0) (0 2))

; Clojuratica handles large numbers without loss of precision, using as little memory as possible.

(math ["bigint" 4938403298403294275025827405847502438570428574320594860842574285743958445309854309574302750296843750]
  "bigint + 1")
;4938403298403294275025827405847502438570428574320594860842574285743958445309854309574302750296843751

; Variables set in the evaluator's binding vector are lexically scoped within the evaluator call:

(math [] "bigint")
;#<Expr bigint>

; Mathematica returned the symbol bigint rather than the number above, indicating that
; bigint was defined only in the scope of the evaluator call that bound it. Internally,
; Clojuratica wraps each call to the evaluator in a Mathematica Module[] and executes the module.

;;;;;; You can get all evaluator output, not just the last expression's

; By passing a flag to the evalutor, you can tell it to return the result of
; every expression in the call, not just the last. The flag can go anywhere among the arguments.

(math [] :all-output
  "foo = 3x + 7 + 2x - (10/2)x"
  "foobar = foo + 3"
  "foobar ^ 2")
;(7 10 100)

; The flag can also be passed to get-evaluator, in which case it will be retained:

(def evaluate* (get-evaluator kernel-link :all-output))
(def math* (comp parse evaluate*))

(math* []
  "foo = 3x + 7 + 2x - (10/2)x"
  "foobar = foo + 3"
  "foobar ^ 2")
;(7 10 100)

;;;;;; Clojuratica is thread-safe

; Rest assured that pieces of Mathematica code evaluated in different threads will not interact.

; Let's define a function containing some Mathematica code that would act erratically if evaluated
; simultaneously in different threads (if Clojuratica were not thread-safe):

(defn count-up []  (math ["counter" 0] 
                     "Do[counter++, {500000}]"
                     "counter"))

; Executing the function in four concurrent threads, we find that the counter variable
; acts reliably. If Clojuratica were not thread-safe, we would expect larger return values.

(pvalues (count-up) (count-up) (count-up) (count-up))
;(500000 500000 500000 500000)

;;;; Global variables and the global setter

; Variables defined in the assignment vector of the evaluator are lexically scoped within that
; call to the evaluator. However, any variables defined in Mathematica expressions have global scope.
; This is a feature, not a bug, but be careful!

(defn count-up2 [] (math [] "Do[counter++, {500000}]"  ; The counter assignment has been removed
                            "counter"))

(evaluate [] "counter = 0")   ; Define counter---in a global scope!

(pvalues (count-up2) (count-up2) (count-up2) (count-up2)) ; This is probably not what you wanted
;(500000 1500000 1000000 2000000)

; Global Mathematica variables can be set explicitly using the *global setter* function:

(def global-set (get-global-setter kernel-link))

; It is good practice to use this function whenever setting globals you care about,
; to avoid confusion like that in count-up2.

(global-set "year" 2009)
;(#<Expr Integer>)

(math [] "year")
; 2009

; Another reason to use the global setter is that it will accept arbitrary Clojure data structures.
; The global setter is the only way to store an arbitrary Clojure data structure as a global
; Mathematica variable. (Reminder: To store a Clojure data structure as a lexically scoped variable,
; use the assignment vector of the evaluator.)

(global-set "recentyears" [2009 2008 2007 2006])
;(#<Expr List> 2009 2008 2007 2006)

(math [] "recentyears - 2000")
;(9 8 7 6)

; A third good reason to use the global setter is that it automatically distributes assignments
; to all parallel kernels. Speaking of which...

;;;;;; Clojuratica supports parallelism

; We saw above that the Clojuratica evaluator is thread-safe. In fact, thread-safety is
; just the beginning.

; Clojuratica can also parallelize. It can distribute Mathemtaica computations from multiple Clojure
; threads to multiple Mathematica kernels, which may be running on multiple cores or multiple
; machines across a cluster or grid. Clojuratica does this by
; adding a thread-ready concurrency layer to Mathematica's built-in parallelism features.

; Using Clojuratica's parallel evaluator, you can have, say, 10,000 Clojure threads all sending different
; expressions to Mathematica. Clojuratica will adaptively farm each expression out to the
; next available Mathematica kernel and ensure that the right Mathematica output is returned
; to the right Clojure thread.
;
; The parallel evaluator has two big selling points. First, kernels are never idle while there
; are expressions left to evaluate.
; Second, the parallel evaluator's concurrency queue is robust to having fewer kernels than threads.
; Threads share kernels adaptively.
; This latter feature is important because Mathematica's kernels are "heavy" memory-wise and are
; limited in number by the availability of Mathematica licenses.
;
; On a hypothetical 50-core machine we would ideally have 50 Mathematica kernels available. The
; Clojuratica concurrency queue will ensure that every expression evaluated by one of our 10,000 Clojure
; threads is farmed out to one of the 50 kernels. While a thread waits for its computation to finish,
; that thread is blocked. The queue, however, is never blocked. Other threads can send expressions to
; Mathematica at any time. A CPU-heavy computation from one thread will not block a CPU-light
; computation from a different thread from finishing on a different kernel.
;
; Mathematica has support for an arbitrarily large number of kernels running on local cores or
; linked over a network. The number of kernels is limited only by the number of licenses available.
; This means that, in theory, you could have tens of thousands of Clojure threads
; evaluating thousands of expressions per minute, all being farmed out to hundreds of Mathematica kernels
; across a cluster or grid.
;
; Because Clojuratica's concurrency queue is built on top of Mathematica's built-in parallelism
; capabilities, the set-up of the parallel Mathematica kernels is done in Mathematica itself:

; <image of setup>

; The same configuration dialog allows the configuration of remote kernels across a cluster or
; the internet.

; The Clojuratica evaluator we created earlier in the tutorial will not parallelize by default.
; To get a parallel evaluator, use the :parallel keyword anywhere in the call to get-evaluator:

(def pevaluate (get-evaluator kernel-link :parallel))   ; Get a parallel evaluator
(def pmath (comp parse pevaluate))    ; Compose a new convenience function for parallel evaluate+parse

; The parallel evaluator has exactly the same syntax as the serial (regular) evaluator:

(pevaluate [] "1 + 1")
;(#<Expr Integer>)

(pmath [] "1 + 1")
;2

; Now we'll test whether parallel evaluation works faster that serial evaluation on
; my dual-core machine.

; First, let's create a vector of thirty agents. These thirty agents are going to end up
; storing the results
; of the thirty CPU-intensive computations we are going to farm out to multiple Mathematica kernels.

(def output-vector
  (vec (for [i (range 30)] (agent nil))))  ; create a vector of thirty agents

; The expression we are going to evaluate thirty times over is a simple integer factorization.
; A single instance of this computation takes about two seconds on my dual-core laptop.

(def expression "FactorInteger[42039483204432094832932317913181713501317130511]")

(time (math [] expression))
;"Elapsed time: 2650.140242 msecs"
;((7 1) (47 1) (2091308860889 1) (2947175658767 1) (20731812453782440993 1))

; To evaluate the expression thirty times over in parallel, we can simply send-off each
; agent with an instruction to update itself with the result of a call to the
; parallel evaluator. Clojure farms out the "send-off" instructions to
; separate threads. This means that the parallel evaluator is receiving expressions from
; thirty different threads at once.

(doseq [output-agent output-vector] ;for each agent in the output vector,
  (send-off output-agent (fn [_] (pmath [] expression))))
                              ;update the agent with the result of the expression

; We can time how long it takes for all thirty expression to complete by monitoring
; the output vector. When it has no more nils in it, all thirty expressions have completed.
; I ran the following command immediately after the doseq command above.

(time
  (while (some #(nil? @%) output-vector)
    (Thread/sleep 50)))
;"Elapsed time: 68123.793457 msecs"

; Since 68,123 ms is shorter than 30 times 2,650 ms (=79,500 ms), it's clear that parallelization is effective.
; The improvement isn't by a factor of two as it should be in theory, but my laptop isn't exactly an
; Azul box.

; Keep in mind that the concurrency queue can handle evaluations from an arbitrarily large 
; number of threads. The overhead for each evaluation is quite low. Let's evaluate 10,000
; CPU-light expressions.

; Create a vector of agents to store the results of 10,000 computations:

(def output-vector
  (vec (for [i (range 10000)] (agent nil))))

; We are going to execute Sqrt[i] 10,000 times, where i is the index of the current agent
; in the output vector. We use "send" instead of "send-off" because my machine doesn't
; have memory for 10,000 threads. A powerful multicore machine or Terracotta cluster might.

(doseq [i (range (count output-vector))] ; for each agent in the output vector
  (send (nth output-vector i)
        (fn [_] (pmath ["i" i] "Sqrt[i] // N"))))
                        ; compute the sqrt of the index of that agent.
                        ; note that //N is Mathematica shorthand for
                        ; "evaluate numerically rather than symbolically"

; Wait for the results:

(time
  (while (some #(nil? @%) output-vector)
    (Thread/sleep 50)))
;"Elapsed time: 63143.038501 msecs"

; This implies that the total overhead per computation is around 6.3 ms, since we computed
; 10,000 expressions and the entire task took 63,000 ms.

; Check that the right thing has happened:
@(nth output-vector 2500)
;50.0

; Note that each each call to the Clojuratica evaluator is sent to a *single*
; kernel. Only when multiple threads are making Mathematica calls simultaneously will the calls be
; distributed to multiple kernels. If you need to parallelize a single Mathematica expression,
; such as a call to Map[], you should use Mathematica's built-in parallelization functions, such
; as Parallelize[].

(math [] "Parallelize[Map[FactorInteger, {1, 11, 111, 1111, 11111}]]")
;(((1 1)) ((11 1)) ((3 1) (37 1)) ((11 1) (101 1)) ((41 1) (271 1)))

; Another quick note: The global setter automatically distributes global variables to all
; parallel kernels.

(global-set "identity" [[1 0] [0 1]])
(pvalues (pmath [] "identity * 2") (pmath [] "identity / 2 // N"))

That covers the features of Clojuratica. Enjoy!

