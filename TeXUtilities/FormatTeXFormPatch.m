(* ::Package:: *)

BeginPackage@"TeXUtilities`FormatTeXFormPatch`"


(* ::Section:: *)
(*Implementation*)


Begin@"`Private`"


ClearAll@"`*"


(* ::Subsection:: *)
(*applyTeXFormFormat*)


applyTeXFormFormat::usage =
"\
applyTeXFormFormat[expr] \
returns expr, wrapped with HoldComplete, with all subexpressions, that have \
custom TeX formating rules, replaced with their TeX form."


applyTeXFormFormat = Function[Null,
	HoldComplete@# /. Flatten@DeleteDuplicates@Cases[
		Unevaluated@#,
		s : Except[HoldPattern@Symbol@___, _Symbol] :>
			Cases[FormatValues@Unevaluated@s,
				HoldPattern[
					HoldPattern[HoldPattern]@Format[x_, TeXForm] :> xTeX_
				] :>
					(HoldPattern@x :> RuleCondition@xTeX)
			],
		{0, Infinity},
		Heads -> True
	],
	HoldAllComplete
]


(* ::Subsection:: *)
(*ExpressionToTeX redefinition*)


Unprotect@Convert`TeX`ExpressionToTeX


Convert`TeX`ExpressionToTeX[expr_, opts___?OptionQ] :=
	Convert`TeX`BoxesToTeX[
		Function[Null, MakeBoxes[#, TraditionalForm], HoldAllComplete] @@
			applyTeXFormFormat@expr,
		opts,
		"BoxRules" -> System`Convert`TeXFormDump`$GreekWords
	]


Protect@Convert`TeX`ExpressionToTeX


End[]


EndPackage[]
