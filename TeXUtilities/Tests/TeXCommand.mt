(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["TeXUtilities`Tests`TeXCommand`", {"MUnit`"}]


<<TeXUtilities`TeXUtilities`


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Function arguments*)


Test[
	TeXCommand[],
	$Failed,
	Message[TeXCommand::argm, TeXCommand, 0, 1],
	TestID -> "no args"
]

Test[
	TeXCommand@a,
	$Failed,
	Message[TeXCommand::string, 1, TeXCommand@a],
	TestID -> "one arg: Symbol"
]
Test[
	TeXCommand@"str" // HoldComplete@#&,
	TeXCommand@"str" // HoldComplete,
	TestID -> "one arg: String"
]

Test[
	TeXCommand["str", a],
	$Failed,
	Message[TeXCommand::list, TeXCommand["str", a], 2],
	TestID -> "two args: String Symbol"
]
Test[
	TeXCommand["str", {a, b}] // HoldComplete@#&,
	TeXCommand["str", {a, b}] // HoldComplete,
	TestID -> "two args: String List"
]
Module[{leaked1 = False, leaked2 = False},
	Test[
		TeXCommand[Unevaluated[leaked1 = True], Unevaluated[leaked2 = True]],
		$Failed,
		Message[TeXCommand::string,
			1, TeXCommand[leaked1 = True, leaked2 = True]
		],
		TestID -> "two args: evaluation leak"
	];
	Test[
		leaked1,
		False,
		TestID -> "two args: evaluation leak: first arg leak"
	];
	Test[
		leaked2,
		False,
		TestID -> "two args: evaluation leak: second arg leak"
	]
]


(* ::Subsection:: *)
(*TeX conversion*)


(* ::Subsubsection:: *)
(*No command arguments*)


Test[
	ToString[TeXCommand@"name", TeXForm],
	"\\name",
	TestID -> "TeX conversion: no command args"
]


(* ::Subsubsection:: *)
(*Non-optional command args*)


Test[
	ToString[TeXCommand["name", {a}], TeXForm],
	"\\name{a}",
	TestID -> "TeX conversion: 1 command arg"
]
Test[
	ToString[TeXCommand["name", {a, b}], TeXForm],
	"\\name{a}{b}",
	TestID -> "TeX conversion: 2 command args"
]


(* ::Subsubsection:: *)
(*Optional command args*)


Test[
	ToString[TeXCommand["name", {{}}], TeXForm],
	"\\name[]",
	TestID -> "TeX conversion: 1 optional command args list: none"
]
Test[
	ToString[TeXCommand["name", {{a, b, c}}], TeXForm],
	"\\name[a,b,c]",
	TestID -> "TeX conversion: 1 optional command args list: symbols"
]
Test[
	ToString[TeXCommand["name", {{a -> b, c -> d}}], TeXForm],
	"\\name[a=b,c=d]",
	TestID -> "TeX conversion: 1 optional command args list: rules"
]
Test[
	ToString[TeXCommand["name", {{a -> b, c, d -> e, f}}], TeXForm],
	"\\name[a=b,c,d=e,f]",
	TestID -> "TeX conversion: 1 optional command args list: symbols and rules"
]
Test[
	ToString[TeXCommand["name", {{a -> b, c}, {d, e -> f}}], TeXForm],
	"\\name[a=b,c][d,e=f]",
	TestID ->
		"TeX conversion: 2 optional command args lists: symbols and rules"
]


(* ::Subsubsection:: *)
(*Mixed command args*)


Test[
	ToString[TeXCommand["name", {{a -> b, c}, d}], TeXForm],
	"\\name[a=b,c]{d}",
	TestID -> "TeX conversion: command args: optional list, arg"
]
Test[
	ToString[TeXCommand["name", {a, {b, c -> d}}], TeXForm],
	"\\name{a}[b,c=d]",
	TestID -> "TeX conversion: command args: arg, optional list"
]
Test[
	ToString[TeXCommand["name", {{a -> b}, c, {d}, e}], TeXForm],
	"\\name[a=b]{c}[d]{e}",
	TestID -> "TeX conversion: command args: moxed optional lists and args"
]


(* ::Subsubsection:: *)
(*"ArgumentConverter" option*)


Test[
	ToString[TeXCommand["name", {{a -> "$b$", "c"}, "\\d"}], TeXForm]
	,
	"\\name[a=\\text{$\\$$b$\\$$},\\text{c}]\
{\\text{$\\backslash \\backslash $d}}"
	,
	TestID -> "TeX conversion: \"ArgumentConverter\" option: default"
]
Test[
	ToString[
		TeXCommand["name", {{a -> "$b$", "c"}, "\\d"},
			"ArgumentConverter" -> ToString
		]
		,
		TeXForm
	]
	,
	"\\name[a=$b$,c]{\\d}",
	TestID -> "TeX conversion: \"ArgumentConverter\" option: ToString"
]
Test[
	ToString[TeXCommand["name", {a, {b -> c}, d},
		"ArgumentConverter" -> ("\\f{" <> ToString@# <> "}"&),
		"ArgumentConverter" -> ToString
	], TeXForm]
	,
	"\\name{\\f{a}}[\\f{b}=\\f{c}]{\\f{d}}"
	,
	TestID -> "TeX conversion: \"ArgumentConverter\" option: two values"
]


(* ::Subsubsection:: *)
(*Evaluation leaks*)


Block[
	{
		leakedLHS = False, leakedRHS = False,
		leakedOpt = False, leakedMand = False
	}
	,
	Test[
		ToString[Unevaluated@TeXCommand["name", {
			{(leakedLHS = True) -> (leakedRHS = True), leakedOpt = True},
			leakedMand = True
		}], TeXForm]
		,
		"\
\\name[\
\\text{leakedLHS}=\\text{True}=\\text{leakedRHS}=\\text{True},\
\\text{leakedOpt}=\\text{True}\
]{\\text{leakedMand}=\\text{True}}"
		,
		TestID -> "TeX conversion: evaluation leak"
	];
	Test[
		leakedLHS,
		False,
		TestID -> "TeX conversion: evaluation leak: optional rule arg LHS leak"
	];
	Test[
		leakedRHS,
		False,
		TestID -> "TeX conversion: evaluation leak: optional rule arg RHS leak"
	];
	Test[
		leakedOpt,
		False,
		TestID -> "TeX conversion: evaluation leak: optional non-rule arg leak"
	];
	Test[
		leakedMand,
		False,
		TestID -> "TeX conversion: evaluation leak: mandatory arg leak"
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest@$ContextPath
