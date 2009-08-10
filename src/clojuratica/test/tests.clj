(use 'clojuratica.clojuratica)
(require '[clojuratica.low-level :as low-level])
(import '[com.wolfram.jlink MathLinkFactory])
(def kernel-link (MathLinkFactory/createKernelLink
          "-linkmode launch -linkname 'c:\\program files\\wolfram research\\mathematica\\7.0\\mathkernel.exe'"))
(.discardAnswer kernel-link)

(def evaluate (get-evaluator kernel-link))
(def mmafn (get-mmafn evaluate))
(def parse (get-parser kernel-link mmafn))
(def math (comp parse evaluate))
(def global-set (get-global-setter evaluate))

(def pevaluate (get-evaluator kernel-link :parallel))
(def pmmafn (get-mmafn pevaluate))
(def pparse (get-parser kernel-link pmmafn))
(def pmath (comp pparse pevaluate))
(def pglobal-set (get-global-setter pevaluate))

(doseq [[evaluate mmafn parse math global-set] [[evaluate  mmafn  parse  math  global-set]
                                                [pevaluate pmmafn pparse pmath pglobal-set]]]
  (assert (= 2 (math [] "1+1")))
  (assert (= '(1 2 3) (math [] "{1,2,3}" :seqs)))
  (assert (= [1 2 3] (math [] "{1,2,3}" :vectors)))
  (assert (= [1 2 3] (math ["x" 3] "{1,2,x}" :vectors)))

  (global-set "twelves" [[12 0] [0 12]])
  (assert (= [[1 0] [0 1]] (math [] "twelves / 12")))

  ;try feeding the core functions equivalent strings, exprs, cexprs
  (let [s     "1"
        expr  (.getExpr (evaluate [] "1"))
        cexpr (evaluate [] "1")]
    (assert (= (parse s)     (parse (evaluate [] s))))
    (assert (= (parse expr)  (parse (evaluate [] expr))))
    (assert (= (parse cexpr) (parse (evaluate [] cexpr)))))

  ;test the conversion and setting of various Clojure data types
  (let [s    "foobar"
        l    (list 1 2 "foobar" 3)
        v    [1 2 "foobar" 3]
        v3d  [[[1 2] [3 4]] [[5 6] [7 8]]]]
    (doseq [obj [s l v v3d]]
      (assert (= obj
                 (math ["obj" obj] "obj")
                 (do (global-set "obj" obj) (math [] "obj"))
                 (parse (low-level/convert obj))))))

  (def increment-me (mmafn ["one" 1] "Function[{x}, x + one]" :no-parse))
  (assert (= 2 (parse (increment-me 1))))

  (def increment-me (mmafn ["one" 1] "Function[{x}, x + one]" :parse))
  (assert (= 2 (increment-me 1)))

  (def fn-generating-fn (mmafn [] "Function[{x}, Table[Function[{y}, y+1], {x}]]" :vectors))
  (def fn-vector (fn-generating-fn 10))
  (assert (= 10 (count fn-vector)))
  (assert (= 0 ((fn-vector 4) -1)))

  )


