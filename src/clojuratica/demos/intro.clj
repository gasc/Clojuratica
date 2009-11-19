

"
I am very happy to announce Clojuratica version 2. Clojuratica now offers the **syntactic** integration of Clojure and Mathematica.

What does that mean? It means you can now write Clojure code that looks like this:

=> (FactorInteger 12345)
[[3 1] [5 1] [823 1]]

That's right, FactorInteger is a Mathematica function. And that's a Clojure REPL.

Symbolic math in Clojure? Syntax-unquoting of Clojure expressions? Absolutely.

=> (Sqrt (* 9 a))
(* 3 (Power a 1/2))

=> (let [x [[2 1] 
            [1 2]]]
     (CholeskyDecomposition ~x))
[[(Power 2 1/2) (Power 2 -1/2)] [0 (Power 3/2 1/2)]]

Note that the nested Clojure vectors are converted on the fly to a Mathematica matrix, and vice versa. Automatic conversions take place for all Clojure and Mathematica data structures.

What's more, Mathematica functions are now Clojure functions. Here is a Mathematica function, expressed as a Clojure expression, that finds the n shortest genes in the human genome:

=> (Function [n] 
     (Take 
       (Sort 
         (Map 
     	     (Function [gene] [(GenomeData gene \"SequenceLength\") gene])
           (GenomeData))) 
       n))
#<parse$parse_fn__1230$fn__1234 clojuratica.base.parse$parse_fn__1230$fn__1234@19fa0b5>

What's that ugly return value? You guessed it: it's a first-class Clojure function. We evaluated a Mathematica function and got back a Clojure function which, when called, will hand off the computation to Mathematica and return the result. 

=> (*1 4)
[[11 "IGHD727"] [16 "IGHD411"] [16 "IGHD417"] [16 "IGHD44"]]

All the power of Mathematica is now seamlessly available in Clojure. You can think of Mathematica as a particularly mature Clojure library for linear algebra, matrix decomposition, symbolic mathematics, optimization, differential equations, symbolic and numerical integration, Fourier analysis, 2D and 3D visualization, image and photo manipulation, exploratory data analysis, probability and statistics, graph theory, number theory, geodesy, and access to the Wolfram Research internet data feeds on finance, chemistry, geometry, meteorology, astronomy, protein structure, and, as we've seen, the human genome.

Let's take a step back and see how this all works.

Observe: Clojure and Mathematica are remarkably similar languages despite their different areas of strength. 

==========     Examples of similarity      ==========

Constant-lookup arrays:
Clj vectors:  [1 2 3]
Mma lists:    {1, 2, 3}

Matrices as nested lists:
Clj:  [[1 0] [0 1]]
Mma:  {{1, 0}, {0, 1}}

Function calls *always* use prefix notation:
Clj:  (func arg1 arg2 arg3)
Mma:  Func[arg1, arg2, arg3]

In Mathematica, common functions do have syntactic sugar, but it always is just syntactic sugar:
Clj:  none
Mma:  1 + 1   is just   Plus[1, 1]
      !foo && (bar > baz)   is just   And[Not[foo], Greater[bar, baz]]

Homoiconicity:
Clj:  (nth '(func arg1 arg2) 1)   ==>   arg1
Mma:  Part[Func[arg1, arg2], 1]   ==>   arg1

=====================================================

These similarities suggest the core idea: Mathematica expressions can be written as Clojure expressions without any loss of information. There is perfect correspondence.

========= Mathematica translated to Clojure =========

Mma:  FactorInteger[1091]         
Clj:  (FactorInteger 1091)

Mma:  Function[{x}, x + 1]
Clj:  (Function [x] (Plus x 1))

=====================================================

The core of Clojuratica is quite simple: convert Clojure expressions to Mathematica expressions, evaluate them in Mathematica, and parse the result back into Clojure expressions. 

There are some other features, too:

- The first-class function integration we saw earlier, wherein Mathematica functions become first-class citizens of Clojure.

- A concurrency queue for multithreaded, parallel computation. Mathematica is not designed for threads or concurrency. It has excellent parallel computation facilities, but parallel computations are initiated from a single-threaded master kernel which blocks until all parallel computations return. By contrast, Clojuratica includes a concurrency queue that lets multiple Clojure threads execute Mathematica expressions without blocking others. The computations will be farmed out to as many Mathematica kernels as are parallelized on the local machine or across a cluster or grid. The computations will return asynchronously, and some threads will go about their business while others continue to wait. I have worked to make the system as high-performance as possible.

- Hashmap conversion. Mathematica has no map data structure, so I include a basic one with Clojuratica. (*This* is not high performance!)

Version 2 of Clojuratica is a complete rewrite of version 1. This should therefore be considered alpha software until a number of people are successfully banging on it. I would appreciate suggestions and bug reports.

I plan to make the integration work in the opposite direction as well when I have time (not for a while!).

Here is an annotated example that illustrates all the features of the software:
"
(comment
(use 'clojure.contrib.repl-ln)
(repl)
(set-repl-prompt "\n> "))

(use 'clojuratica)

; These five lines initialize the Mathematica kernel and create an active KernelLink object. These lines are dictated by Mathematica's J/Link library. The fourth line is platform-specific. Please read the Mathematica J/Link documentation for details on how to initialize the kernel on your platform. 
(import '[com.wolfram.jlink MathLinkFactory])
(def kernel-link
		(MathLinkFactory/createKernelLink
			"-linkmode launch -linkname 'c:\\program files\\wolfram research\\mathematica\\7.0\\mathkernel.exe'"))
(.discardAnswer kernel-link)
; Create a math-evaluate function. The main purpose of the math-evaluate function is to be handed off to the math-intern and defmath macros. The math-evaluator function, which returns math-evalute, encloses kernel-link inside math-evaluate. It also accepts any and all of the flags we will see later: if it is given any, it closes over them, too.
(def math-evaluate (math-evaluator kernel-link))
; Now we are going to define the math macro. The math macro is a general purpose macro that reads and evaluates Mathematica expressions. The first argument to defmath is the name of the new math macro. The second argument is the math-evaluate function we created a second ago.
(defmath math math-evaluate)
; Using the math macro we can do some math.
(math (Plus 1 1))
(math (FactorInteger 12345))
; By default, Clojuratica defines a number of function aliases. Thus, even though Plus is the real name of the Mathematica function for summing, you can write the first command above in a more familiar form:
(math (+ 1 1))
; Likewise for other functions. A full list of aliases is reproduced later. Here are some:
(math (&& True False))
(math (= f 4))    ; this is Mathematica's Set function, which is for assignment
(math f)
; If you will be using certain Mathematica functions frequently, you may want to intern them as macros. math-intern does this.
(math-intern math-evaluate Plus [FI FactorInteger])
; The first argument is the math-evaluate function we created earlier. The remaining arguments are operation specifications (opspecs). The opspecs of math-intern can be of three types: a single symbol, a vector of two symbols, or a string. The last will be discussed later. An opspec made up of a symbol interns the corresponding Mathematica function using that very name, as seen by the following:
(Plus [1 2] [3 4])  ; Plus does element-wise addition of vectors
; An opspec vector of two symbols interns the second funtion by the first name, as seen by the following:
(FI 12345)
; Note that these math-interned functions are Clojure **macros**. Therefore, unquoted Mathematica expressions and symbols can be passed as arguments:
(Plus (* 4 x) (+ 20 2) (AnyFunction 3))
; The math macro, and any function math-interned as a macro, accept flags. The following is an illustration of the :no-parse flag, which stops Clojuratica from parsing the return value back into a Clojure expression. Instead, an object of class com.wolfram.JLink.Expr is returned.
(Plus :no-parse (* 4 x) (+ 20 2) (AnyFunction 3))
; Similarly, the :no-evaluate flag stops the evaluation step:
(math :no-evaluate (* 4 x) (+ 20 2) (AnyFunction 3))
; The Clojuratica math-evaluator (which underlies the math macro and the math-interned functions) supports around 30 flags and options. These are detailed at the end.



