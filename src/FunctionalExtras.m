(* ::Package:: *)

BeginPackage["FunctionalExtras`"];


SetAttributes[Assert, HoldAllComplete];

Assert[bool_] :=
	If[!bool,
		Throw["Assertion false: " <> ToString[Defer[bool] // StandardForm]]]


SetAttributes[Asserting, HoldAllComplete];

Asserting[bool_, body_] :=
	(Assert[bool];
	 body)


SetAttributes[Let, HoldAllComplete];

Let[{firstassignment_, restassignments__}, body_] := 
	With[{firstassignment}, 
		Let[{restassignments}, body]];
Let[{assignment_}, body_] := 
	With[{assignment}, body];
Let[{}, body_] := body;

Let::usage = "Let[{varname = value, ...}, body] is identical to With[{varname = value, ...}, body] except " <>
             "later assignments can \"see\" earlier assignments; no nesting required.";


Every[pred_, list_List] := 
	Apply[And, Map[pred, list]];

Every::usage = "Every[pred, list] returns True if and only if pred is True for every element in list.";


Some[pred_, list_List] := 
	Apply[Or, Map[pred, list]];

Some::usage = "Some[pred, list] returns True if and only if pred is True for at least one element in list.";


Second[list_List] := 
	Part[list, 2];

Second::usage = "Second[list] returns the second element in list.";


Reshape[list_List, {}] := Flatten[list];


Reshape[list_List, dimensions : {__}] :=
	Fold[Partition, list, Reverse[dimensions][[1 ;; -2]]];


FullTranspose[list_List] :=
	Transpose[list, Reverse[Table[i, {i, 1, ArrayDepth[list]}]]];


EndPackage[];
