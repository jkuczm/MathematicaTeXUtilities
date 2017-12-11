(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["TeXUtilities`Tests`TeXVerbatim`", {"MUnit`"}]


<<TeXUtilities`TeXUtilities`


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Arguments*)


Test[
	TeXVerbatim[],
	$Failed,
	Message[TeXVerbatim::argx, TeXVerbatim, 0],
	TestID -> "no args"
]

Test[
	TeXVerbatim@a,
	$Failed,
	Message[TeXVerbatim::string, 1, TeXVerbatim@a],
	TestID -> "one arg: symbol"
]
Test[
	TeXVerbatim@"str" // HoldComplete@#&,
	TeXVerbatim@"str" // HoldComplete,
	TestID -> "one arg: string"
]

Test[
	TeXVerbatim["str1", "str2"],
	$Failed,
	Message[TeXVerbatim::argx, TeXVerbatim, 2],
	TestID -> "two args"
]
Module[{leaked1 = False, leaked2 = False},
	Test[
		TeXVerbatim[Unevaluated[leaked1 = True], Unevaluated[leaked2 = True]],
		$Failed,
		Message[TeXVerbatim::argx, TeXVerbatim, 2],
		TestID -> "two args: evaluation leaks"
	];
	Test[
		leaked1,
		False,
		TestID -> "two args: evaluation leaks: first arg leak"
	];
	Test[
		leaked2,
		False,
		TestID -> "two args: evaluation leaks: second arg leak"
	]
]


(* ::Subsection:: *)
(*TeXForm*)


Test[
	ToString[TeXVerbatim@"simple string", TeXForm],
	"simple string",
	TestID -> "TeXForm: simple string"
]
Test[
	ToString[TeXVerbatim@"\"something\"", TeXForm],
	"\"something\"",
	TestID -> "TeXForm: string with double quotes"
]
Test[
	ToString[TeXVerbatim@"\\command{arg} $a + b^c$", TeXForm],
	"\\command{arg} $a + b^c$",
	TestID -> "TeXForm: string with TeX special characters"
]
Test[
	ToString[TeXVerbatim@"", TeXForm],
	"",
	TestID -> "TeXForm: empty string"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest@$ContextPath
