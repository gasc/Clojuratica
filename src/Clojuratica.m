(* ::Package:: *)

(*
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
; Contributor (s):
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
*)


BeginPackage["Clojuratica`", {"JLink`"}]


Off[SetDelayed::write]  (* Loading a Java class with protected fields (i.e. *any Clojure object with state) 
                           produces a harmless message. Disable the message. *)


Clojure::usage = 
	"Clojure@obj@method[args], Clojure@Class`staticmethod[args], Clojure@obj@field, Clojure@Class`staticfield, and " <> 
	"Clojure [ JavaNew[\"Classname\",args] ] all call Java in the usual way, except that they first translate all arguments " <>
	"to Clojure data structures to the extent possible and afterwards translate all return values and accessed field " <> 
	"values back into Mathematica expressions.";


(*
Clojure is the name of a set of (overloaded) functions that provide the main interface to
Clojure and Java. Clojure can be called with five kinds of argument, namely the five ways one can
access Java from Mathematica. These are:

JavaNew["Classname", args]
obj@method[args]
Class`staticmethod[args]
obj@field
Class`staticfield

In the first three cases, Clojure translates all arguments into native Clojure data structures
(to the extent possible) before passing them on to the class or object in question. The final four cases, 
Clojure translates into a native Mathematica expression the method's return value or the 
accessed field value.

Clojure can be called using standard syntax. For example:

Clojure[ Class`staticmethod[args] ]

However, it is conventional and preferred to use prefix syntax. For example:

Clojure@Class`staticmethod[args]
Clojure@obj@method[args]
*)


Clojure@object_Symbol@method_Symbol[args___] /; JavaObjectQ[object] :=
	With[{convertedArgSeq = ConvertIntoSeq[args]},
		object@method[convertedArgSeq] // ReturnAsJavaObject // ClojureParse]


Clojure[staticMethod_[args___]] :=
	With[{convertedArgSeq = ConvertIntoSeq[args]},
		staticMethod[convertedArgSeq] // ReturnAsJavaObject // ClojureParse]


Clojure@object_Symbol@field_Symbol /; JavaObjectQ[object] := 
	object@field // ReturnAsJavaObject // ClojureParse


Clojure@staticField_Symbol := 
	staticField // ReturnAsJavaObject // ClojureParse


Clojure@JavaNew[class_String, args___] :=
	With[{convertedArgSeq = ConvertIntoSeq[args]},
		JavaNew[class, convertedArgSeq]]


SetAttributes[Clojure, HoldAll]


ClojureRun::usage = "foo";


ClojureRun = Composition[ClojureParse, ClojureEvaluate];


ClojureEvaluate::usage = 
	"ClojureEvaluate[string, ...] evaluates in an embedded Clojure compiler the Clojure expression(s) contained in string(s). " <>
  "clojuratica.clojuratica is automatically required and com.wolfram.jlink is automatically imported. The return value is " <>
  "a Java or Clojure object, which you can parse with ClojureParse if you wish. Note that the embedded Clojure compiler is " <>
  "SLOW. Do not use this interface for production work. Instead, use gen-class to create a Java class and call it with " <>
  "Clojure[...]. Takes an option AllOutput, false by default. If it's false, ClojureEvaluate returns only the result of the final " <>
  "expression. If it's true, ClojureEvaluate returns a list containing the result of every expression. Note that the first " <>
  "invocation of ClojureEvaluate will take a LONG time as Clojure is loaded. Call EmbedClojure[] ahead of time to " <>
  "relocate this delay.";


ClojureEvaluate[str__String, OptionsPattern[]] :=
	With[{},
		SendRead["(require 'clojuratica.clojuratica)"];
		SendRead["(import '[com.wolfram.jlink])"];
		If[OptionValue[AllOutput],
			Map[SendRead, List[str]],
			With[{},
				Scan[SendRead, List[str][[;;-2]]];
				List[str][[-1]] // SendRead]] ]


Options[ClojureEvaluate] = {AllOutput -> False}


(* Parse a Clojure data structure into a Mathematica expr *)
ClojureParse[obj_] := 
	With[{},
		InstallJava[];
		LoadJavaClass["clojuratica.CLink"];
		obj // CLink`parse]


(* Parse a Clojure data structure into a Mathematica expr *)
ClojureParse[{objs__}] := 
	With[{},
		InstallJava[];
		LoadJavaClass["clojuratica.CLink"];
		Map[CLink`parse, {objs}]]


(* Convert a Mathematica expr into a Clojure data structure *)
ClojureConvert[expr_] := 
	With[{},
		InstallJava[];
		LoadJavaClass["clojuratica.CLink"];
		expr // MakeJavaExpr // CLink`convert // ReturnAsJavaObject]


ClojureSetGlobal::usage = 
	"";


ClojureSetGlobal[var_String, value_] :=
	With[{
			clojureFn = "(fn get-mathematica-var [varname] 
									   (let [evaluate (clojuratica.clojuratica/get-evaluator 
																			(com.wolfram.jlink.StdLink/getLink))
													 parse (clojuratica.clojuratica/get-parser)]
											 (parse (evaluate [] varname))))"},
		Clojuratica`globalTransferer = value;
		SendRead["(def " <> var <> " (" <> clojureFn <> " \"Clojuratica`globalTransferer\"))"]]


EmbedClojure[] :=
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
		LoadJavaClass["clojure.lang.XMLHandler"];]


Begin["`Private`"]


SendRead[str_String] :=
	With[{},
		InstallJava[];
		EmbedClojure[];
		LoadJavaClass["clojure.lang.Compiler"];
		LoadJavaClass["java.io.StringReader"];
		LoadJavaClass["clojuratica.CLink"];
		CLink`mirrorClasspath[];
		With[{stringReader = JavaNew["java.io.StringReader", str]},
			clojure`lang`Compiler`load[stringReader] // ReturnAsJavaObject]]


ConvertIntoSeq[args___] :=
	With[{convertedArgs = Map[ClojureConvert, {args}]},
		Apply[Sequence, convertedArgs]]


End[]


EndPackage[]
