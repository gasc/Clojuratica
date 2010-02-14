(* ::Package:: *)

BeginPackage["FunctionalExtras`"]


Inc; Dec; Output; Assert; Let; Assign; ThreadThrough; MapThrough; EveryQ; AnyQ; NotAnyQ; NotEveryQ; Some; FalseishQ; TrueishQ; 
Second; Third;


Begin["`Private`"]


Inc[x_] := x + 1
Dec[x_] := x - 1


Output[x_] := (Print[x]; x)
Output[s_String, x_] := (Print[s, x]; x)


SetAttributes[Assert, HoldFirst]

Assert[bool_] :=
	If[!bool,
		Throw["Assertion false: " <> ToString[Defer[bool] // StandardForm]]]

Assert[bool_, str_String] :=
	If[!bool,
		Throw[str]]


SetAttributes[Let, HoldAll]

Let[{firstassignment_, restassignments___}, body_] := 
	With[{firstassignment}, 
		Let[{restassignments}, body]]

Let[{}, body_] := body

Let[{{firstname_, restnames___} = {val_, restvals___}, restassignments___}, body_] := 
	Let[{firstname = val},
		Let[{{restnames} = {restvals}}, 
			Let[{restassignments}, body]]]

Let[{{names___} = {vals___}, restassignments___}, body_] := 
	Let[{restassignments}, body]


SetAttributes[Assign, HoldAll]
Off[Module::lvlist]

Assign[{f_[firstname_, v_] /; f === Set || f === SetDelayed, rest___}, body_] :=
	With[{value = v},
		ReleaseHold[
			ReleaseHold[
				Hold[
					Module[FunctionalExtras`Assign`assignment, 
						Hold[f[firstname, value]];
						Hold[Assign[{rest}, body]]]] /. 
					HoldPattern[FunctionalExtras`Assign`assignment] -> (Hold[{firstname}] //. {x___, {y___},z___} :> {x, y, z})]]]

Assign[{firstname_, rest___}, body_] :=
	With[{value = v},
		ReleaseHold[
			ReleaseHold[
				Hold[
					Module[FunctionalExtras`Assign`assignment, 
						Hold[Assign[{rest}, body]]]] /. 
					HoldPattern[FunctionalExtras`Assign`assignment] -> (Hold[{firstname}] //. {x___, {y___},z___} :> {x, y, z})]]]

Assign[{}, body_] := body


ThreadThrough[x_, forms__] :=
	Fold[#2[#1]&, x, {forms}]


MapThrough[x_, forms__] :=
	Fold[Map[#2, #1]&, x, {forms}]

MapThrough[x_, level_Integer, forms__] :=
	Fold[Map[#2, #1, level]&, x, {forms}]

MapThrough[x_, level_List, forms__] :=
	Fold[Map[#2, #1, level]&, x, {forms}]


EveryQ[list_List, pred_] := 
	!AnyQ[list, !pred[#]&]


NotEveryQ[list_List, pred_] := 
	!EveryQ[list, pred]


Some[list_List, pred_] := 
	Let[{result = Select[list, pred, 1]},
		If[Length[result] == 0, False, result // First]]


AnyQ[list_List, pred_] := 
	Length[Select[list, pred, 1]] != 0


NotAnyQ[list_List, pred_] := 
	!AnyQ[list, pred]


FalseishQ[b_] := b === False || b === Null


TrueishQ[b_] := !Falseish[b]


Second[list_List] := list[[2]]

Second::usage = "Second[list] returns the second element in list."


Third[list_List] := list[[3]]

Third::usage = "Third[list] returns the third element in list."


End[]


EndPackage[]


(*SetAttributes[Bind, HoldAllComplete]

Bind[{Set[firstname_, first_], restassignments___}, body_] := 
	ReleaseHold[Hold[Bind[{restassignments}, body]] //. HoldPattern[firstname] -> first]

Bind[{}, body_] := body

Bind[{Set[{firstname_, restnames___}, v_], restassignments___}, body_] := 
	With[{
			len = Length[v]},
		With[{
				first = If[len == 0, Null, First[v]],
				rest = If[len == 0, {}, Rest[v]]},
			Bind[{firstname = first},
				Bind[{Set[{restnames}, rest]}, 
					Bind[{restassignments}, body]]]]]

Bind[{Set[{}, v_], restassignments___}, body_] := 
	Bind[{restassignments}, body]

Bind::usage = "Bind[{varname -> value, ...}, body] is identical to With[{varname -> value, ...}, body] except " <>
                 "later assignments can \"see\" earlier assignments; no nesting required."*)


(*Collapse[data_, by_, fs_] :=
	Let[{
			gathered=GatherBy[data,Part[#,by]&],
			collapsed=MapThread[#1[#2]&,{fs,Transpose[#]}]& /@ gathered},
		collapsed]*)


(*Reshape[list_List, {}] := Flatten[list]*)


(*Reshape[list_List, dimensions : {__}] :=
	Fold[Partition, list, Reverse[dimensions][[1 ;; -2]]]*)


(*FullTranspose[list_List] :=
	Transpose[list, Reverse[Table[i, {i, 1, ArrayDepth[list]}]]]*)


(*SetAttributes[Let2, HoldAllComplete]

Off[Module::lvlist]
Off[Module::lvsym]
Let2[assignments_, body__]:=
	(ReleaseHold[Module[vv,CompoundExpression[aa,body]]
		/.vv->
			vv[{ReleaseHold[
				HoldComplete[assignments]
					/. HoldPattern[Set[x_, _]]:>HoldComplete[x]
					//. HoldComplete[y___,{x___},z___]:>HoldComplete[y,x,z]]}]
		/.aa->
			ReplacePart[HoldComplete[assignments],CompoundExpression,{1,0}]]
	//.vv[{a___,x_,b___,x_,c___}]:>vv[{a,b,x,c}]
	/.vv[x_]->x)*)


(*BeginPackage["foo`"]
Let5
Begin["`Private`"]
SetAttributes[Let5,HoldAllComplete]
Let5[{assignment_,restassignments___},body__]:=
ReleaseHold[Module[zzz,HoldComplete[assignment];Let5[{restassignments},body]]/.
zzz->{Extract[HoldComplete[assignment],ConstantArray[1,Depth[HoldComplete[assignment]]-1],HoldComplete]}]
Let5[{},body_]:=body
End[]
(*aaa->ReplacePart[HoldComplete[{assignments}],CompoundExpression,{1}]]*)
EndPackage[]*)


(*SetAttributes[Bind,HoldAllComplete]
Bind[{___,e_,___},_List,___,Set[e_,_],___]:=Throw["Binding outside block!"]
Bind[{___,e_,___},_List,___,SetDelayed[e_,_],___]:=Throw["Binding outside block!"]
Bind[{e___},{g : Set[f_,_]...?MemberQ[{e},f]},___]:=Throw[1]
Bind[a_,{b___},c___]:=Module[a,CompoundExpression[b,c]]*)


(*
Off[With::lvlist]
SetAttributes[Let2, HoldAllComplete]

Let2[{Set[firstname_, v_], rest___}, body_] :=
	With[{value = v},
		ReleaseHold[
			ReleaseHold[
				Hold[
					With[FunctionalExtras`Let2`aaa,
						Hold[Let2[{rest}, body]]]] /.
					HoldPattern[FunctionalExtras`Let2`aaa] -> Hold[{firstname = v}]]]]

Let2[{}, body_] := body

Let2[{Set[{firstname_, restnames___}, v_], restassignments___}, body_] := 
	With[{len = Length[v]},
		With[{
				first = If[len == 0, Null, First[v]],
				rest = If[len == 0, {}, Rest[v]]},
			Let2[{firstname = first},
				Let2[{Set[{restnames}, rest]}, 
					Let2[{restassignments}, body]]]]]

Let2[{Set[{}, v_], restassignments___}, body_] := 
	Let2[{restassignments}, body]


SetAttributes[Let, HoldAllComplete]
Let[{firstassignment_, restassignments___}, body_] := 
	With[{firstassignment}, 
		Let[{restassignments}, body]]
Let[{}, body_] := body
Let[{Set[{firstname_, restnames___}, v_], restassignments___}, body_] := 
	With[{
			value = v, 
			len = Length[v]},
		With[{
				first = If[len == 0, Null, First[v]],
				rest = If[len == 0, {}, Rest[v]]},
			Let[{firstname = first},
				Let[{Set[{restnames}, rest]}, 
					Let[{restassignments}, body]]]]]
Let[{Set[{}, v_], restassignments___}, body_] := 
	Let[{restassignments}, body]
*)
