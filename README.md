# Clojuratica #

An interface between Clojure and Wolfram Mathematica.

## What is Clojuratica? ##

Clojuratica brings together two of today's most exciting tools for high-performance, parallel computation.

[Clojure](http://clojure.org) is a new dynamic programming language with a compelling approach to concurrancy and state, exciting facilities for functional programming and immutability, and a growing reputation for doing all the right things. [Wolfram Mathematica](https://www.wolfram.com/mathematica/) is arguably the world's most powerful integrated tool for numerical computation, symbolic mathematics, optimization, and visualization and is build on top of its own splendid functional programming language.

By linking the two:

* CLojuratica lets you **write and evaluate Mathematica code in Clojure** with full **syntactic integration**. Now Clojure programs can take advantage of Mathematica's enormous range of numerical and symbolic mathematics algorithms and fast matrix algebra routines.
* Clojuratica provides the **seamless and transparent translation of native data structures** between Clojure and Mathematica. This includes high-precision numbers, matricies, N-dimensional arrays, and evaluated and unevaluated Mathematica expressions and formulae.
* Clojuratica lets you **call, pass, and store Mathematica functions just as if they were first-class functions in Clojure.** This is high-level functional programming at its finest. You can write a function in whichever language is more suited to the task and never think again about which platform is evaluating calls to that function.
* Clojuratica facilitates **the "Clojurization" of Mathematica's existing parallel-computing capabilities.** Mathematica is not designed for threads or concurrency. It has excellent support for parallel computation, but parallel evaluations are initiated from a single-threaded master kernel which blocks until all parallel evaluations return. By contrast, Clojuratica includes a concurrency framework that lets multiple Clojure threads execute Mathematica expressions without blocking others. Now it is easy to run a simulation in Clojure with 1,000 independent threads asynchronously evaluating processor-intensive expressions in Mathematica. The computations will be farmed out adaptively and transparently to however many Mathematica kernels are available on any number of processor cores, either locally or across a cluster, grid, or network.

Clojuratica is open-source and targeted at applications in scientific computing, computational economics, finance, and other fields that rely on the combination of parallelized simulation and high-performance number-crunching. Clojuratica gives the programmer access to Clojure's most cutting-edge features--easy concurrency and multithreading, immutable persistent data structures, and software transactional memory---alongside Mathematica's easy-to-use algorithms for numerics, symbolic mathematics, optimization, statistics, visualization, and image-processing.

The canonical pronunciation of Clojuratica starts with Clojure and rhymes with erotica.

## Author ##

Clojuratica was created by Garth Sheldon-Coulson, a graduate student at the Massachusetts Institute of Technology and Harvard Law School. See the [Community](http://clojuratica.weebly.com/community.html) page to find out how to contribute to Clojuratica, suggest features, report bugs, or ask general questions.

## License ##

Clojuratica is open-source software. It is released under the "Mozilla tri-license," otherwise known as the MPL/GPL/LGPL triple license. Permission is granted to use the software under the terms of any one of:

* The [Mozilla Public License, version 1.1][MPL] or later (MPL)
* The [GNU General Public License, version 2][GPL] or later (GPL)
* The [GNU Lesser General Public License, version 2.1][LGPL] or later (LGPL)

[MPL]: http://www.mozilla.org/MPL/MPL-1.1.html
[GPL]: http://www.gnu.org/licenses/gpl-2.0.html
[LGPL]: http://www.gnu.org/licenses/lgpl-2.1.html

## Legal ##

The product names used in this web site are for identification purposes only. All trademarks and registered trademarks, including "Wolfram Mathematica," are the property of their respective owners. Clojuratica is not a product of Wolfram Research. The software on this site is provided "as-is," without any express or implied warranty.

