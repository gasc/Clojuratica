(* ::Package:: *)

(* 
The problem with this attempt at embedding the REPL
was that the embedded REPL could not, for whatever reason,
correctly ascertain the class of a given object.
*)


(*
ClojureParse[{objs__}] := 
	With[{},
		InstallJava[];
		LoadJavaClass["clojuratica.CHelper"];
		Map[clojuratica`CHelper`parse, {objs}]];
*)


(*ClojureRun = Composition[ClojureParse, ClojureEvaluate];*)


(*
ClojureEvaluate::usage = 
	"ClojureEvaluate[string, ...] evaluates in an embedded Clojure compiler the Clojure expression(s) contained in string(s). " <>
  "clojuratica.clojuratica is automatically required and com.wolfram.jlink is automatically imported. The return value is " <>
  "a Java or Clojure object, which you can parse with ClojureParse if you wish. Note that the embedded Clojure compiler is " <>
  "SLOW. Do not use this interface for production work. Instead, use gen-class to create a Java class and call it with " <>
  "Clojure[...]. Takes an option AllOutput, false by default. If it's false, ClojureEvaluate returns only the result of the final " <>
  "expression. If it's true, ClojureEvaluate returns a list containing the result of every expression. Note that the first " <>
  "invocation of ClojureEvaluate will take a LONG time as Clojure is loaded. Call EmbedClojure[] ahead of time to " <>
  "relocate this delay.";
*)


(*
ClojureEvaluate[str__String, OptionsPattern[]] :=
	With[{},
		SendRead["(require 'clojuratica.clojuratica)"];
		SendRead["(import '[com.wolfram.jlink])"];
		If[OptionValue[AllOutput],
			Map[SendRead, List[str]],
			With[{},
				Scan[SendRead, List[str][[;;-2]]];
				List[str][[-1]] // SendRead]]];
*)


(*Options[ClojureEvaluate] = {AllOutput -> False};*)


(*
SendRead[str_String] :=
	With[{},
		InstallJava[];
		InstallClojure[];
		LoadJavaClass["clojure.lang.Compiler"];
		LoadJavaClass["java.io.StringReader"];
		LoadJavaClass["clojuratica.CHelper"];
		CHelper`mirrorClasspath[];
		With[{stringReader = JavaNew["java.io.StringReader", str]},
			Clojuratica`Private`compiler@load[stringReader] // ReturnAsJavaObject]];
*)


(*
ClojureSetGlobal[var_String, value_] :=
	With[{
			clojureFn = "(fn get-mathematica-var [varname] 
									   (let [evaluate (clojuratica.clojuratica/get-evaluator 
																			(com.wolfram.jlink.StdLink/getLink))
													 parse (clojuratica.clojuratica/get-parser)]
											 (parse (evaluate [] varname))))"},
		Clojuratica`Private`globalTransferer = value;
		SendRead["(def " <> var <> " (" <> clojureFn <> " \"Clojuratica`Private`globalTransferer\"))"]];
*)


(*
InstallClojure[] :=
	With[{},
		InstallJava[];
		LoadClojure[];
		If[Not[JavaObjectQ[Clojuratica`Private`compiler]],
			Clojuratica`Private`compiler = JavaNew["clojure.lang.Compiler"];]];
*)


(*
ReinstallClojure[] :=
	With[{},
		ReinstallJava[];
		LoadClojure[];
		Clojuratica`Private`compiler = JavaNew["clojure.lang.Compiler"];];
*)


(*
LoadClojure[] :=
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
		LoadJavaClass["clojure.lang.XMLHandler"];];
*)
