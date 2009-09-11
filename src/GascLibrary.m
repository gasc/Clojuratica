(* ::Package:: *)

BeginPackage["GascLibrary`"];


Let[{firstassignment_, restassignments__}, body_] := 
	With[{firstassignment}, 
		Let[{restassignments}, body]];
Let[{assignment_}, body_] := 
	With[{assignment}, body];
SetAttributes[Let, HoldAll];

Let::usage = "Let[{varname = value, ...}, body] is identical to With[{varname = value, ...}, body] except " <>
             "later assignments can \"see\" earlier assignments; no nesting required.";


Every[pred_, list_] := 
	Apply[And, Map[pred, list]];

Every::usage = "Every[pred, list] returns True if and only if pred is True for every element in list.";


Some[pred_, list_] := 
	Apply[Or, Map[pred, list]];

Some::usage = "Some[pred, list] returns True if and only if pred is True for at least one element in list.";


(* Begin hash map code *)


(* Return all pairs *)
HashMapObject[data_Symbol][] := 
	Map[#[[1,1,1]] -> #[[2]]&, DownValues[data]];

(* Return value by key, specifying return value if key not found *)
HashMapObject[data_Symbol][key_, else_] :=
	With[{value = data[key]},
		If[Head[value] =!= data, value, else]];

(* Return value by key *)
HashMapObject[data_Symbol][key_] := 
	HashMapObject[data][key, Null];


(* Associate element returning new map *)
HashMapObject /: Associate[HashMapObject[data_Symbol], newelements___Rule] := 
	AssociateWith[HashMap[HashMapObject[data][]], newelements];

Associate::usage = "Associate[hashmap, key -> value, ...] returns a new hashmap with key/value pair(s) added.";

(* Associate element in place *)
HashMapObject /: AssociateWith[HashMapObject[data_Symbol], newelements___Rule] := 
 (Scan[(data[First[#]] = Last[#]) &, {newelements}];
	HashMapObject[data]);

AssociateWith::usage = "AssociateWith[hashmap, key -> value, ...] adds key/value pair(s) to hashmap in place.";


(* Dissociate element returning new map *)
HashMapObject /: Dissociate[HashMapObject[data_Symbol], keys___] := 
	DissociateFrom[HashMap[HashMapObject[data][]], keys];

Dissociate::usage = "Dissociate[hashmap, key, ...] returns a new hashmap with key(s) and associated values removed.";

(* Dissociate element in place *)
HashMapObject /: DissociateFrom[HashMapObject[data_Symbol], keys___] := 
 (Scan[(data[#] =.) &, {keys}];
	HashMapObject[data]);

DissociateFrom::usage = "DissociateFrom[hashmap, key, ...] removes key(s) and associated values from hashmap in place.";


HashMapObject /: Format[HashMapObject[data_Symbol]] :=
	"<<HashMapObject[" <> ToString[HashMapObject[data][]] <> "]>>";

HashMapObject /: MakeBoxes[HashMapObject[data_Symbol], format_] := 
	Let[{databox = ToBoxes[HashMapObject[data][]]},
		InterpretationBox[
			RowBox[{"\[LeftGuillemet]", RowBox[{"HashMapObject", "[", databox, "]"}], "\[RightGuillemet]"}], 
			HashMapObject[data]]];


(* Create new map *)
HashMap[elements___Rule] :=
	Module[{HashMapData},
		AssociateWith[HashMapObject[HashMapData], elements]];

(* Create new map *)
HashMap[{elements___Rule}] := 
	HashMap[elements];

HashMap::usage = "HashMap[] returns a new empty hashmap. \n" <> 
                 "HashMap[key -> value, ...] returns a new hashmap containing the key/value pair(s). \n" <>
                 "HashMap[{}] and HashMap[{key -> value, ...}] do the same.";


(* Create new map *)
ZipMap[keys_, values_] :=
	HashMap[MapThread[Rule, {keys, values}]];

ZipMap::usage = "ZipMap[keys, values] returns a new hashmap with the elements of list keys mapped to the elements of list values.";


(* End hash map code *)


EndPackage[];
