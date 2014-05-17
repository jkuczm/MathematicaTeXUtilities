(* ::Package:: *)

BeginPackage["TeXUtilities`"]


(* ::Section:: *)
(*Usage messages*)


TeXVerbatim::usage =
"\
TeXVerbatim[\"str\"] \
converted to TeX gives verbatim \"str\"."


(* ::Section:: *)
(*Implementation*)


(*
	Unprotect all symbols in this context
	(all public symbols provided by this package)
*)
Unprotect["`*"]


Begin["`Private`"]


(* ::Subsection:: *)
(*TeXVerbatim*)


TeXVerbatim[arg:Except[_String]] := (
	Message[TeXVerbatim::string, 1, HoldForm[TeXVerbatim[arg]]];
	$Failed
)

TeXVerbatim[args___] :=
	With[
		{argsNo = Length[{args}]}
		,
		(
			Message[
				TeXVerbatim::argx,
				HoldForm[TeXVerbatim],
				HoldForm[argsNo]
			];
			$Failed
		)
			/; argsNo =!= 1
	]


System`Convert`TeXFormDump`maketex[
	RowBox[{
		"TeXVerbatim",
		"(" | "[", 
		arg_String?(StringMatchQ[#, "\"" ~~ ___ ~~ "\""] &), 
		")" | "]"
	}]] :=
		ToExpression[arg]


End[]


(* ::Section:: *)
(*Public symbols protection*)


(*
	Protect all symbols in this context
	(all public symbols provided by this package)
*)
Protect["`*"]


EndPackage[]
