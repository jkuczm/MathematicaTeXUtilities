(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


Begin["TestEnvironment`TeXCommand`"];


Get["TeXUtilities`TeXUtilities`"];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Function arguments*)


Test[
	TeXCommand[],
	$Failed,
	{HoldForm @ Message[TeXCommand::argm, HoldForm[TeXCommand], 0, 1]},
	TestID -> "no args"
];

Test[
	TeXCommand[a],
	$Failed,
	{HoldForm[Message[TeXCommand::string, 1, HoldForm[TeXCommand[a]]]]},
	TestID -> "one arg: Symbol"
];
TestMatch[
	TeXCommand["str"],
	HoldPattern[TeXCommand["str"]],
	TestID -> "one arg: String"
];

TestMatch[
	TeXCommand["str", a],
	$Failed,
	{HoldForm[Message[TeXCommand::list, HoldForm[TeXCommand["str", a]], 2]]},
	TestID -> "two args: String Symbol"
];
TestMatch[
	TeXCommand["str", {a, b}],
	HoldPattern[TeXCommand["str", {a, b}]],
	TestID -> "two args: String List"
];


(* ::Subsection:: *)
(*TeX converion*)


(* ::Subsubsection:: *)
(*No command argumentss*)


Test[
	ToString[TeXCommand["name"], TeXForm],
	"\\name",
	TestID -> "TeX converion: no command args"
];


(* ::Subsubsection:: *)
(*Non-optional command args*)


Test[
	ToString[TeXCommand["name", {a}], TeXForm],
	"\\name{a}",
	TestID -> "TeX converion: 1 command arg"
];
Test[
	ToString[TeXCommand["name", {a, b}], TeXForm],
	"\\name{a}{b}",
	TestID -> "TeX converion: 2 command args"
];


(* ::Subsubsection:: *)
(*Optional command args*)


Test[
	ToString[TeXCommand["name", {{}}], TeXForm],
	"\\name[]",
	TestID -> "TeX converion: 1 optional command args list: none"
];
Test[
	ToString[TeXCommand["name", {{a, b, c}}], TeXForm],
	"\\name[a,b,c]",
	TestID -> "TeX converion: 1 optional command args list: symbols"
];
Test[
	ToString[TeXCommand["name", {{a -> b, c -> d}}], TeXForm],
	"\\name[a=b,c=d]",
	TestID -> "TeX converion: 1 optional command args list: rules"
];
Test[
	ToString[TeXCommand["name", {{a -> b, c, d -> e, f}}], TeXForm],
	"\\name[a=b,c,d=e,f]",
	TestID -> "TeX converion: 1 optional command args list: symbols and rules"
];
Test[
	ToString[TeXCommand["name", {{a -> b, c}, {d, e -> f}}], TeXForm],
	"\\name[a=b,c][d,e=f]",
	TestID -> "TeX converion: 2 optional command args lists: symbols and rules"
];


(* ::Subsubsection:: *)
(*Mixed command args*)


Test[
	ToString[TeXCommand["name", {{a -> b, c}, d}], TeXForm],
	"\\name[a=b,c]{d}",
	TestID -> "TeX converion: command args: optional list, arg"
];
Test[
	ToString[TeXCommand["name", {a, {b, c -> d}}], TeXForm],
	"\\name{a}[b,c=d]",
	TestID -> "TeX converion: command args: arg, optional list"
];
Test[
	ToString[TeXCommand["name", {{a -> b}, c, {d}, e}], TeXForm],
	"\\name[a=b]{c}[d]{e}",
	TestID -> "TeX converion: command args: moxed optional lists and args"
];


(* ::Subsubsection:: *)
(*"ArgumentConverter" option*)


Test[
	ToString[TeXCommand["name", {{a -> "$b$", "c"}, "\\d"}], TeXForm]
	,
	"\\name[a=\\text{$\\$$b$\\$$},\\text{c}]\
{\\text{$\\backslash \\backslash $d}}"
	,
	TestID -> "TeX converion: \"ArgumentConverter\" option: default"
];
Test[
	ToString[
		TeXCommand[
			"name", {{a -> "$b$", "c"}, "\\d"}, "ArgumentConverter" -> ToString
		]
		,
		TeXForm
	]
	,
	"\\name[a=$b$,c]{\\d}",
	TestID -> "TeX converion: \"ArgumentConverter\" option: ToString"
];


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"];
Quiet[Remove["`*"], {Remove::rmnsm}];


End[];
