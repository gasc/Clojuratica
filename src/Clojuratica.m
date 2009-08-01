(* ::Package:: *)

BeginPackage["Clojuratica`", {"JLink`"}]


Off[SetDelayed::write]  (* Loading a Java class with protected fields (i.e. *any Clojure object with state) 
                           produces a harmless message. Disable the message. *)


ClojureCall[object_Symbol@method_Symbol[args___]] :=
	With[{parsedArgSeq = ParseIntoSeq[args]},
		object@method[parsedArgSeq] // ReturnAsJavaObject // Convert]


ClojureCall[staticMethod_Symbol[args___]] :=
	With[{parsedArgSeq = ParseIntoSeq[args]},
		staticMethod[parsedArgSeq] // ReturnAsJavaObject // Convert]


ClojureCall[object_Symbol@field_Symbol] := 
	object@field // ReturnAsJavaObject // Convert


ClojureCall[staticField_Symbol] := 
	staticField // ReturnAsJavaObject // Convert


ClojureCall[JavaNew[class_, args___]] :=
	With[{parsedArgSeq = ParseIntoSeq[args]},
		JavaNew[class, parsedArgSeq]]


SetAttributes[ClojureCall, HoldAll]


ClojureEvaluate[str__String, OptionsPattern[]] :=
	If[OptionValue[AllOutput],
		Map[Composition[Convert, Eval], List[str]],
		With[{},
			Scan[Eval, List[str][[;;-2]]];
			List[str][[-1]] // Eval // Convert]] 


Options[ClojureEvaluate] = {AllOutput -> False}


DefGlobal[var_String, value_] :=
	With[{
			clojureFn = "(fn get-mathematica-var [varname] 
									   (let [evaluate (clojuratica.clojuratica/get-evaluator 
																			(com.wolfram.jlink.StdLink/getLink))
													 parse (clojuratica.clojuratica/get-parser)]
											 (parse (evaluate [] varname))))"},
		Clojuratica`globalTransferer = value;
		Eval["(def " <> var <> " (" <> clojureFn <> " \"Clojuratica`globalTransferer\"))"]]


InstallClojure[] :=
	With[{},
		InstallJava[];
		LoadJavaClass["clojure.lang.AFn"];
		LoadJavaClass["clojure.lang.AFunction"];
		LoadJavaClass["clojure.lang.Agent"];
		LoadJavaClass["clojure.lang.AMapEntry"];
		LoadJavaClass["clojure.lang.APersistentMap"];
		LoadJavaClass["clojure.lang.APersistentSet"];
		LoadJavaClass["clojure.lang.APersistentVector"];
		LoadJavaClass["clojure.lang.ARef"];
		LoadJavaClass["clojure.lang.AReference"];
		LoadJavaClass["clojure.lang.ArraySeq"];
		LoadJavaClass["clojure.lang.ArrayStream"];
		LoadJavaClass["clojure.lang.ASeq"];
		LoadJavaClass["clojure.lang.Associative"];
		LoadJavaClass["clojure.lang.Atom"];
		LoadJavaClass["clojure.lang.Binding"];
		LoadJavaClass["clojure.lang.Box"];
		LoadJavaClass["clojure.lang.Compile"];
		LoadJavaClass["clojure.lang.Compiler"];
		LoadJavaClass["clojure.lang.Cons"];
		LoadJavaClass["clojure.lang.Counted"];
		LoadJavaClass["clojure.lang.Delay"];
		LoadJavaClass["clojure.lang.DynamicClassLoader"];
		LoadJavaClass["clojure.lang.EnumerationSeq"];
		LoadJavaClass["clojure.lang.Fn"];
		LoadJavaClass["clojure.lang.IDeref"];
		LoadJavaClass["clojure.lang.IFn"];
		LoadJavaClass["clojure.lang.IMapEntry"];
		LoadJavaClass["clojure.lang.IMeta"];
		LoadJavaClass["clojure.lang.IndexedSeq"];
		LoadJavaClass["clojure.lang.IObj"];
		LoadJavaClass["clojure.lang.IPersistentCollection"];
		LoadJavaClass["clojure.lang.IPersistentList"];
		LoadJavaClass["clojure.lang.IPersistentMap"];
		LoadJavaClass["clojure.lang.IPersistentSet"];
		LoadJavaClass["clojure.lang.IPersistentStack"];
		LoadJavaClass["clojure.lang.IPersistentVector"];
		LoadJavaClass["clojure.lang.IProxy"];
		LoadJavaClass["clojure.lang.IReduce"];
		LoadJavaClass["clojure.lang.IRef"];
		LoadJavaClass["clojure.lang.IReference"];
		LoadJavaClass["clojure.lang.ISeq"];
		LoadJavaClass["clojure.lang.IteratorSeq"];
		LoadJavaClass["clojure.lang.IteratorStream"];
		LoadJavaClass["clojure.lang.Keyword"];
		LoadJavaClass["clojure.lang.LazilyPersistentVector"];
		LoadJavaClass["clojure.lang.LazySeq"];
		LoadJavaClass["clojure.lang.LineNumberingPushbackReader"];
		LoadJavaClass["clojure.lang.LispReader"];
		LoadJavaClass["clojure.lang.LockingTransaction"];
		LoadJavaClass["clojure.lang.MapEntry"];
		LoadJavaClass["clojure.lang.MultiFn"];
		LoadJavaClass["clojure.lang.Named"];
		LoadJavaClass["clojure.lang.Namespace"];
		LoadJavaClass["clojure.lang.Numbers"];
		LoadJavaClass["clojure.lang.Obj"];
		LoadJavaClass["clojure.lang.PersistentArrayMap"];
		LoadJavaClass["clojure.lang.PersistentHashMap"];
		LoadJavaClass["clojure.lang.PersistentHashSet"];
		LoadJavaClass["clojure.lang.PersistentList"];
		LoadJavaClass["clojure.lang.PersistentQueue"];
		LoadJavaClass["clojure.lang.PersistentStructMap"];
		LoadJavaClass["clojure.lang.PersistentTreeMap"];
		LoadJavaClass["clojure.lang.PersistentTreeSet"];
		LoadJavaClass["clojure.lang.PersistentVector"];
		LoadJavaClass["clojure.lang.ProxyHandler"];
		LoadJavaClass["clojure.lang.Range"];
		LoadJavaClass["clojure.lang.Ratio"];
		LoadJavaClass["clojure.lang.Ref"];
		LoadJavaClass["clojure.lang.Reflector"];
		LoadJavaClass["clojure.lang.Repl"];
		LoadJavaClass["clojure.lang.RestFn"];
		LoadJavaClass["clojure.lang.Reversible"];
		LoadJavaClass["clojure.lang.RT"];
		LoadJavaClass["clojure.lang.Script"];
		LoadJavaClass["clojure.lang.Seqable"];
		LoadJavaClass["clojure.lang.SeqEnumeration"];
		LoadJavaClass["clojure.lang.SeqIterator"];
		LoadJavaClass["clojure.lang.Sequential"];
		LoadJavaClass["clojure.lang.Settable"];
		LoadJavaClass["clojure.lang.Sorted"];
		LoadJavaClass["clojure.lang.Stream"];
		LoadJavaClass["clojure.lang.Streamable"];
		LoadJavaClass["clojure.lang.StringSeq"];
		LoadJavaClass["clojure.lang.Symbol"];
		LoadJavaClass["clojure.lang.TransactionalHashMap"];
		LoadJavaClass["clojure.lang.Util"];
		LoadJavaClass["clojure.lang.Var"];
		LoadJavaClass["clojure.lang.XMLHandler"]]


Begin["`Private`"]


(* Convert a Clojure data structure into a Mathematica expr *)
Convert[obj_] := 
	With[{},
		InstallJava[];
		LoadJavaClass["clojuratica.CLink"];
		obj // CLink`convert]


(* Parse a Mathematica expr into a Clojure data structure *)
Parse[expr_] := 
	With[{},
		InstallJava[];
		LoadJavaClass["clojuratica.CLink"];
		expr // MakeJavaExpr // CLink`parse // ReturnAsJavaObject]


Eval[str_] :=
	With[{},
		InstallJava[];
		InstallClojure[];
		LoadJavaClass["clojure.lang.Compiler"];
		LoadJavaClass["java.io.StringReader"];
		LoadJavaClass["clojuratica.CLink"];
		CLink`mirrorClasspath[];
		With[{
				Send = 
					Function[{toSend},
						With[{stringReader = JavaNew["java.io.StringReader", toSend]},
							clojure`lang`Compiler`load[stringReader] // ReturnAsJavaObject]]},
			Send["(require 'clojuratica.clojuratica)"];
			Send["(import '[com.wolfram.jlink])"];
			Send[str]]]


ParseIntoSeq[args___] :=
	With[{parsedArgs = Map[Parse, {args}]},
		Apply[Sequence, parsedArgs]]


End[]


EndPackage[]
