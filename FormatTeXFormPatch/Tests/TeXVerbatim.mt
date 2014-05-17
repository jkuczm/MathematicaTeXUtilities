(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


Begin["TestEnvironment`TeXVerbatim`"];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Arguments*)


Test[
	TeXVerbatim[],
	$Failed,
	{HoldForm[Message[TeXVerbatim::argx, HoldForm[TeXVerbatim], HoldForm[0]]]},
	TestID -> "no args"
];

Test[
	TeXVerbatim[a],
	$Failed,
	{HoldForm[Message[TeXVerbatim::string, 1, HoldForm[TeXVerbatim[a]]]]},
	TestID -> "one arg: symbol"
];
TestMatch[
	TeXVerbatim["str"],
	HoldPattern[TeXVerbatim["str"]],
	TestID -> "one arg: string"
];

Test[
	TeXVerbatim["str1", "str2"],
	$Failed,
	{HoldForm[Message[TeXVerbatim::argx, HoldForm[TeXVerbatim], HoldForm[2]]]},
	TestID -> "two args"
];


(* ::Subsection:: *)
(*TeXForm*)


Test[
	ToString[TeXVerbatim["simple string"], TeXForm],
	"simple string",
	TestID -> "TeXForm: simple string"
];
Test[
	ToString[TeXVerbatim["\"something\""], TeXForm],
	"\"something\"",
	TestID -> "TeXForm: string with double quotes"
];
Test[
	ToString[TeXVerbatim["\\command{arg} $a + b^c$"], TeXForm],
	"\\command{arg} $a + b^c$",
	TestID -> "TeXForm: string with TeX special characters"
];
Test[
	ToString[TeXVerbatim[""], TeXForm],
	"",
	TestID -> "TeXForm: empty string"
];


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"];
Quiet[Remove["`*"], {Remove::rmnsm}];


End[];
