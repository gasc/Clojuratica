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


BeginPackage["Clojuratica`", {"JLink`"}];


Off[SetDelayed::write];  (* Loading a Java class with protected fields (i.e. *any Clojure object with state) 
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
		object@method[convertedArgSeq] // ReturnAsJavaObject // ClojureParse];


Clojure@staticMethod_[args___] :=
	With[{convertedArgSeq = ConvertIntoSeq[args]},
		staticMethod[convertedArgSeq] // ReturnAsJavaObject // ClojureParse];


Clojure@object_Symbol@field_Symbol /; JavaObjectQ[object] := 
	object@field // ReturnAsJavaObject // ClojureParse;


Clojure@staticField_Symbol := 
	staticField // ReturnAsJavaObject // ClojureParse;


Clojure@JavaNew[class_String, args___] :=
	With[{convertedArgSeq = ConvertIntoSeq[args]},
		JavaNew[class, convertedArgSeq]];


LoadJavaClass["com.wolfram.jlink.StdLink"];
LoadJavaClass["clojuratica.CLink"];


Clojuratica`fnWrap = False;
Clojuratica`evaluator = CLink`getEvaluator[StdLink`getLink[], ""];


SetAttributes[Clojure, HoldFirst];


(* Parse a Clojure data structure into a Mathematica expr *)
ClojureParse[obj_] := 
	With[{},
		InstallJava[];
		LoadJavaClass["clojuratica.CHelper"];
		obj // clojuratica`CHelper`parse];


(* Convert a Mathematica expr into a Clojure data structure *)
ClojureConvert[expr_] := 
	With[{},
		InstallJava[];
		LoadJavaClass["clojuratica.CHelper"];
		If[JavaObjectQ[expr], 
			expr,
			clojuratica`CHelper`convert[expr // MakeJavaExpr, Clojuratica`fnWrap, Clojuratica`evaluator] // ReturnAsJavaObject]];


Begin["`Private`"];


ConvertIntoSeq[args___] :=
	With[{convertedArgs = Map[ClojureConvert, {args}]},
		Apply[Sequence, convertedArgs]];


End[];


EndPackage[];
