(* ::Package:: *)

BeginPackage["FormatTeXFormPatch`"]


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
(*Private symbols usage*)


ApplyTeXFormFormat::usage =
"\
ApplyTeXFormFormat[expr] \
returns expr with all subexpressions that have custom TeX formating rules \
replaced with their TeX form wrapped with TeXVerbatim."


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


(* ::Subsection:: *)
(*ApplyTeXFormFormat*)


ApplyTeXFormFormat[expr_] :=
	expr /.
		Flatten[
			Cases[
				(* Get formating rules, *)
				FormatValues[#]
				,
				(* take only rules defined for TeXFormat *)
				HoldPattern[
					HoldPattern[HoldPattern][Format[x_, TeXForm]] :> xTeX_
				] :>
					(* and change them to replacement rules for bare
						transformed expression. *)
					(HoldPattern[x] :> xTeX)
			] & /@
				(* Perform above actions for all symbols found in expr. *)
				DeleteDuplicates[
					Cases[expr, _Symbol, {0, Infinity}, Heads -> True]
				]
		]


(* ::Subsection:: *)
(*ExpressionToTeX redefinition*)


Unprotect[Convert`TeX`ExpressionToTeX];


Convert`TeX`ExpressionToTeX[expr_, opts___?OptionQ] :=
	Convert`TeX`BoxesToTeX[
		With[
			{formatedExpr = ApplyTeXFormFormat[expr]}
			, 
			MakeBoxes[formatedExpr, TraditionalForm]
		]
		,
		opts
		,
		"BoxRules" -> System`Convert`TeXFormDump`$GreekWords
	]


Protect[Convert`TeX`ExpressionToTeX];


End[]


(* ::Section:: *)
(*Public symbols protection*)


(*
	Protect all symbols in this context
	(all public symbols provided by this package)
*)
Protect["`*"]


EndPackage[]
