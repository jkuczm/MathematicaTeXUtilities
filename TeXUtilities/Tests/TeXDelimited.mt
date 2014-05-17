(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


Begin["TestEnvironment`TeXDelimited`"];


Get["TeXUtilities`TeXUtilities`"];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Function arguments*)


Test[
	TeXDelimited[],
	$Failed,
	{HoldForm[Message[TeXDelimited::argm, HoldForm[TeXDelimited], 0, 2]]},
	TestID -> "0 args"
];

Test[
	TeXDelimited["str"],
	$Failed,
	{HoldForm[Message[TeXDelimited::argm, HoldForm[TeXDelimited], 1, 2]]},
	TestID -> "1 arg: String"
];


Test[
	TeXDelimited["str", a],
	$Failed,
	{HoldForm @ Message[
		TeXDelimited::string, 2, HoldForm[TeXDelimited["str", a]]
	]},
	TestID -> "2 args: String Symbol"
];
Test[
	TeXDelimited[a, "str"],
	$Failed,
	{HoldForm @ Message[
		TeXDelimited::string, 1, HoldForm[TeXDelimited[a, "str"]]
	]},
	TestID -> "2 args: Symbol String"
];
TestMatch[
	TeXDelimited["str1", "str2"],
	HoldPattern[TeXDelimited["str1", "str2"]],
	TestID -> "2 args: Strings"
];


Test[
	TeXDelimited["str", a, b],
	$Failed,
	{HoldForm @ Message[
		TeXDelimited::string, 3, HoldForm[TeXDelimited["str", a, b]]
	]},
	TestID -> "3 args: String Symbol Symbol"
];
Test[
	TeXDelimited[a, b, "str"],
	$Failed,
	{HoldForm @ Message[
		TeXDelimited::string, 1, HoldForm[TeXDelimited[a, b, "str"]]
	]},
	TestID -> "3 args: Symbol Symbol String"
];
TestMatch[
	TeXDelimited["str1", a, "str2"],
	HoldPattern[TeXDelimited["str1", a, "str2"]],
	TestID -> "3 args: String Symbol String"
];


Test[
	TeXDelimited["str", a, b, "Indentation" -> ""],
	$Failed,
	{HoldForm @ Message[
		TeXDelimited::string,
		3,
		HoldForm[TeXDelimited["str", a, b, "Indentation" -> ""]]
	]},
	TestID -> "4 args: String Symbol Symbol, explicit option"
];


(* ::Subsection:: *)
(*TeX converion*)


(* ::Subsubsection:: *)
(*Basic*)


Test[
	ToString[TeXDelimited["\\left", "\\right"], TeXForm]
	,
	"\
\\left
\\right"
	,
	TestID -> "TeX converion: no body"
];

Test[
	ToString[TeXDelimited["\\left", "body", "\\right"], TeXForm]
	,
	"\
\\left
    \\text{body}
\\right"
	,
	TestID -> "TeX converion: 1 body expression"
];

Test[
	ToString[TeXDelimited["\\left", "str", sym, "\\right"], TeXForm]
	,
	"\
\\left
    \\text{str}
    \\text{sym}
\\right"
	,
	TestID -> "TeX converion: 2 body expressions"
];


(* ::Subsubsection:: *)
(*Nested*)


Test[
	ToString[
		TeXDelimited["\\left1",
			TeXDelimited["\\left2", "body", "\\right2"],
		"\\right1"]
		,
		TeXForm
	]
	,
	"\
\\left1
    \\left2
        \\text{body}
    \\right2
\\right1"
	,
	TestID -> "TeX converion: nested: 2 levels"
];
Test[
	ToString[
		TeXDelimited["\\left1",
			"expr1",
			TeXDelimited["\\left2",
				TeXDelimited["\\left3","expr2", "\\right3"],
				"expr3",
			"\\right2"],
			"expr4",
		"\\right1"]
		,
		TeXForm
	]
	,
	"\
\\left1
    \\text{expr1}
    \\left2
        \\left3
            \\text{expr2}
        \\right3
        \\text{expr3}
    \\right2
    \\text{expr4}
\\right1"
	,
	TestID -> "TeX converion: nested: 3 levels with other expressions"
];


(* ::Subsubsection:: *)
(*Options*)


Test[
	ToString[
		TeXDelimited["\\left",
			"expr1",
			"expr2",
			"\\right",
			"BodyConverter" -> ToString
		]
		,
		TeXForm
	]
	,
	"\
\\left
    expr1
    expr2
\\right"
	,
	TestID -> "TeX converion: \"BodyConverter\" option: ToString"
];

Test[
	ToString[
		TeXDelimited[
			"\\left",
			"expr1",
			"expr2",
			"expr3",
			"\\right",
			"BodySeparator" -> " "
		]
		,
		TeXForm
	]
	,
	"\
\\left
    \\text{expr1} \\text{expr2} \\text{expr3}
\\right"
	,
	TestID -> "TeX converion: \"BodySeparator\" option: space"
];

Test[
	ToString[
		TeXDelimited[
			"\\left",
			"expr1",
			"expr2",
			"expr3",
			"\\right",
			"DelimSeparator" -> " "
		]
		,
		TeXForm
	]
	,
	"\
\\left \\text{expr1}
    \\text{expr2}
    \\text{expr3} \\right"
	,
	TestID -> "TeX converion: \"DelimSeparator\" option: space"
];

Test[
	ToString[
		TeXDelimited[
			"\\left1",
			"expr1",
			TeXDelimited[
				"\\left2", "expr2", "\\right2",
				"Indentation" -> "  "
			],
			"expr3",
			"\\right1",
			"Indentation" -> "\t"
		]
		,
		TeXForm
	]
	,
	"\
\\left1
\t\\text{expr1}
\t\\left2
\t  \\text{expr2}
\t\\right2
\t\\text{expr3}
\\right1"
	,
	TestID -> "TeX converion: \"Indentation\" option: \t, two spaces"
];


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"];
Quiet[Remove["`*"], {Remove::rmnsm}];


End[];
