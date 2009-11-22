; Read the one on the web site, not this one... the text here is not kept up to date.

(use 'clojure.contrib.repl-ln)
(repl)
(set-repl-prompt "\n=> ")

; ----- Setup -----
; Let's get started by using the package. Be sure you have followed the installation directions on the Get page of the Clojuratica web site.
(use 'clojuratica)
; The following three commands initialize the Mathematica kernel and create an active KernelLink object. This API is dictated by Mathematica's J/Link library. The second command in particular is platform-specific. Please read the Mathematica J/Link documentation at http://reference.wolfram.com/mathematica/JLink/tutorial/WritingJavaProgramsThatUseMathematica.html#15574 for details on how to initialize the kernel on your platform.
(import '[com.wolfram.jlink MathLinkFactory])
(def kernel-link
     (MathLinkFactory/createKernelLink
       "-linkmode launch -linkname 'c:/program files/wolfram research/mathematica/7.0/mathkernel.exe'"))
(.discardAnswer kernel-link)
; Create a math evaluator called math-evaluate. The main purpose of the math evaluator is to be handed off to the math-intern and def-math-macro macros below. 
(def math-evaluate (math-evaluator kernel-link '(ParallelNeeds "init`")))
;(def math-evaluate (math-evaluator kernel-link))
; Now we are going to define the math macro. The math macro is a general purpose macro that reads and evaluates Mathematica expressions. The first argument to def-math-macro is the name of the new math macro. The second argument is the math evaluator we created a second ago (we called it math-evaluate).
(def-math-macro math math-evaluate)
; Using the math macro we can do some math!
(math (Plus 1 1))
(math (FactorInteger 12345))
; The math macro accepts more than one form. It encloses the forms in an implicit CompoundExpression, which is Mathematica's equivalent of Clojure's do. (Mathematica programmers will recall that Mathematica's syntactic sugar for CompoundExpression is infix semicolons.)
(math 
     (Plus 1 1)
     (FactorInteger 12345))
; ----- Aliases -----
; By default, Clojuratica defines a number of function aliases. Thus, even though Plus is the real name of the Mathematica function for summing, we can write the first command above in a more familiar form:
(math (+ 1 1))
; Likewise for other functions. A full list of aliases is reproduced in the API documentation. Here are some more examples:
(math (&& True False))
(math (= f 4))    ; this is Mathematica's Set function, for assignment
(math f)
; An important alias is "do" for CompoundExpression. Note the lowercase! This is directly analagous to Clojure's do.
(math
     (Plus 1
           (do
             (= f 1)
             f)))
; ----- Interning -----
; If we will be using certain Mathematica functions frequently, we may want to intern them as macros. math-intern does this.
(math-intern math-evaluate Plus [FI FactorInteger])
; The first argument above is the math evaluator we created earlier. The remaining arguments are intern specs. Each intern spec may be a single symbol, a vector of two symbols, or a string. An intern spec made up of a symbol interns the corresponding Mathematica function using that very name, as seen by the following:
(Plus [1 2] [3 4])  ; look, Plus does element-wise addition of vectors
; An intern spec made up of a vector of two symbols interns the second function by the first name, as seen by the following:
(FI 12345)
; Note that math-intern interns Mathematica functions as Clojure **macros**. Therefore, unquoted Mathematica expressions and symbols can be passed as arguments:
(Plus (* 4 x) (+ 20 2) (AnyFunction 3))
Plus
; The intern spec form we haven't discussed yet is the string:
(math-intern math-evaluate "System`Factor*")
; This intern spec form uses the Names function of Mathematica to get all symbols matching the string, interning them all. Any matching symbols that would cause an error are silently ignored (the Integer function, for instance, cannot be interned because it conflicts with the Java class Integer.) Yes, it's possible to say (math-intern math-evaluate "System`*") to intern all of Mathematica. This seems like overkill, but you're welcome to it.
; If we would like to intern only Mathematica's scoping constructs (Function, Module, Block, With, Let, the last added by Clojuratica), we can use the :scopes flag:
(math-intern math-evaluate :scopes)
; It is also possible to intern Mathematica funcions as Clojure **functions**. The trade-off is we will need to quote all symbols and won't be able to use reader macros (discussed later). On the other hand we will be able to use the Mathematica function as a first-class Clojure function:
(math-intern :as-function math-evaluate [PlusFn Plus])
PlusFn
(map #(apply PlusFn %) [[1 2] [3 4 'a] [5 6]])
; As it turns out, if we are willing to quote all our symbols and forgo reader macros (discussed later), we can use the math evaluator directly the same way we have been using the math macro:
(math-evaluate (list 'Plus 3 4 'a))
; ----- Flags -----
; The math macro and any math-interned operator accept math evaluator flags. The following is an illustration of the :no-parse math evaluator flag, which stops Clojuratica from parsing the return value back into a Clojure expression. Instead, an object of class com.wolfram.JLink.Expr is returned.
(Plus :no-parse (* 4 x) (+ 20 2) (AnyFunction 3))
; Similarly, the :no-evaluate flag stops the evaluation step:
(math :no-evaluate (* 4 x) (+ 20 2) (AnyFunction 3))
; Any flag may be anywhere in the top level of the form:
(Plus (* 4 x) (+ 20 2) :no-parse (AnyFunction 3))
;
(Plus (* 4 x) (+ 20 2) :no-parse :parse (AnyFunction 3))
;
(math :clojure-aliases {} (+ 1 1))
;
(def math-evaluate* (math-evaluator :no-parse kernel-link))
(def-math-macro math* math-evaluate*)
(math* 1)
; ----- Reader Macros -----
; Within the math macro and any functions math-interned as macros, the following reader macros are available.
; Postfix calls:
(math ^(a b c d)) ; same as Mathematica's a // b // c // d
; Prefix calls:
(math @(a b c d)) ; same as Mathematica's a @ b @ c @ d
; Anonymous functions with args. Note the apostrophe! This is not #(...)!:
(math #'(+ % %2)) ; same as Mathematica's Plus[#, #2] &
(*1 1 2)
; Inline Mathematica syntax:
(math (+ 1 '"{1, Sqrt[4], 3+4}"))
; ----- Implicit Do -----
; Mathematica's scoping constructs (Function, Module, Block, With, Let, the last added by Clojuratica) do **not** have an implicit do. If Clojuratica did not provide a solution, you would have to write Mathematica scopes like this:
(Block [x y z]
     (do
       (Something x)
       (SomethingElse y)
       (YetMore z)))
; Instead, Clojuratica ships with versions of the scoping constructs that provide an implicit do. These are automatically aliased to Function, Module, etc., so you don't have to worry about changing your Clojure habits. You can write the above like this:
(Block [x y z]
     (Something x)
     (SomethingElse y)
     (YetMore z))
; ----- Data Structure Conversion -----
; Clojuratica converts Clojure vectors to Mathematica lists and vice versa:
(math (Head [1 2 3]))
(math (List 1 2 3))
; If you want Clojuratica to parse lists to seqs instead of vectors, use the :seqs flag:
(math :seqs [1 2 3])
; Mathematica expressions are always parsed to Clojure lists, so when using :seqs you can always distinguish expressions from the seqs:
(math :seqs [[1 2 3] (MyExpression arg1 arg2 arg3)])
[(type (first *1)) (type (second *1))]
; Clojuratica converts hashmaps to a HashMapObject type included with Clojuratica. These types can be created with HashMap. They are functions of their values. To get all values in a HashMapObject, evaluate it as a function of no arguments. Conversion to and parsing from HashMapObjects can be turned off with the :no-hash-maps flag.
(math (Head {a b c d}))
(math ({a b c d} c))
(math ({a b c d}))
(math (HashMap [(-> a b) (-> c d)]))
(math :no-hash-maps {a b c d})
(math :no-hash-maps (Head {a b c d}))
; ----- Parallel Evaluation -----
; First, it should be said that Mathematica's built-in parallelization functions are excellent for single-threaded applications. Go ahead and use them from Clojure.
(math (LaunchKernels))
(math (ParallelEvaluate $KernelID))
(math (ParallelMap #'(Plus % 1) [1 2 3 4]))
; If we have multiple threads making calls to Mathematica, we may want these calls to be handled by different parallel Mathematica kernels in an asynchronous fashion. Clojuratica provides an easy facility for this. Simply execute the call using the :parallel flag:
(math :parallel (+ 1 1))
; A real example follows. Note that the return value of the let is a list of the Mathematica $KernelIDs of the kernels on which each call to f executes. This list allows us to see the parallelization explicitly.
(time
     (let [f      (math :parallel (Function [x] (Pause 0.5) $KernelID))
           agents (take 10 (repeatedly #(agent nil)))]
       (doall (map #(send-off % f) agents))
       (doall (map await agents))
       (map deref agents)))
; Note that different calls to f are evaluated by different parallel kernels (I have just two cores on my machine).
; To get asynchronous parallel execution we must use the :parallel flag **and** make the parallel calls concurrently from different threads, as above. If we replace our agents with atoms and thereby stay in a single thread, we get serial execution:
(time 
     (let [f      (math :parallel (Function [x] (Pause 0.5) $KernelID))
           atoms  (take 10 (repeatedly #(atom nil)))]
       (doall (map #(swap! % f) atoms))
       (map deref atoms)))
; Likewise, if return to agents but omit the :parallel flag, we again get serial execution:
(time
     (let [f      (math (Function [x] (Pause 0.5) $KernelID))
           agents (take 10 (repeatedly #(agent nil)))]
       (doall (map #(send-off % f) agents))
       (doall (map await agents))
       (map deref agents)))
; Clojuratica is always and everywhere thread-safe. Each call to the math evaluator (including via the math macro and math-interned functions) is executed atomically.


