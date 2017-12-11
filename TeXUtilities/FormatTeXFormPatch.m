(* ::Package:: *)

BeginPackage@"TeXUtilities`FormatTeXFormPatch`"


(* ::Section:: *)
(*Implementation*)


Begin@"`Private`"


ClearAll@"`*"


(* ::Subsection:: *)
(*Private symbols usage*)


applyTeXFormFormat::usage =
"\
applyTeXFormFormat[expr] \
returns expr with all subexpressions, that have custom TeX formating rules, \
replaced with their TeX form."


(* ::Subsection:: *)
(*applyTeXFormFormat*)


applyTeXFormFormat@expr_ := expr /. Flatten[
	Cases[
		(* Get formating rules, *)
		FormatValues[#]
		,
		(* take only rules defined for TeXFormat *)
		HoldPattern[HoldPattern[HoldPattern]@Format[x_, TeXForm] :> xTeX_] :>
			(* and change them to replacement rules for bare
				transformed expression. *)
			(HoldPattern@x :> xTeX)
	] & /@
		(* Perform above actions for all symbols found in expr. *)
		DeleteDuplicates@Cases[expr, _Symbol, {0, Infinity}, Heads -> True]
]


(* ::Subsection:: *)
(*ExpressionToTeX redefinition*)


Unprotect@Convert`TeX`ExpressionToTeX


Convert`TeX`ExpressionToTeX[expr_, opts___?OptionQ] :=
	Convert`TeX`BoxesToTeX[
		ToBoxes[applyTeXFormFormat@expr, TraditionalForm],
		opts,
		"BoxRules" -> System`Convert`TeXFormDump`$GreekWords
	]


Protect@Convert`TeX`ExpressionToTeX


End[]


EndPackage[]
