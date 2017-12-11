(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["TeXUtilities`Tests`TeXEnvironment`", {"MUnit`"}]


<<TeXUtilities`TeXUtilities`


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Function arguments*)


Test[
	TeXEnvironment[],
	$Failed,
	Message[TeXEnvironment::argm, TeXEnvironment, 0, 1],
	TestID -> "no args"
]

Test[
	TeXEnvironment@a,
	$Failed,
	Message[TeXEnvironment::StrOrListWithStr, TeXEnvironment@a],
	TestID -> "one arg: Symbol"
]
Test[
	TeXEnvironment@"str" // HoldComplete@#&,
	TeXEnvironment@{"str"} // HoldComplete,
	TestID -> "one arg: String"
]
Test[
	TeXEnvironment@{},
	$Failed,
	Message[TeXEnvironment::StrOrListWithStr, TeXEnvironment@{}],
	TestID -> "one arg: empty List"
]
Test[
	TeXEnvironment@{a},
	$Failed,
	Message[TeXEnvironment::StrOrListWithStr, TeXEnvironment@{a}],
	TestID -> "one arg: List with Symbol"
]
Test[
	TeXEnvironment@{"str"} // HoldComplete@#&,
	TeXEnvironment@{"str"} // HoldComplete,
	TestID -> "one arg: List with String"
]

Test[
	TeXEnvironment["str", a] // HoldComplete@#&,
	TeXEnvironment[{"str"}, a] // HoldComplete,
	TestID -> "two args: String Symbol"
]
Test[
	TeXEnvironment["str", a, b] // HoldComplete@#&,
	TeXEnvironment[{"str"}, a, b] // HoldComplete,
	TestID -> "three args: String Symbol Symbol"
]


(* ::Subsection:: *)
(*TeX conversion*)


(* ::Subsubsection:: *)
(*Basic*)


Test[
	ToString[TeXEnvironment["name", "body"], TeXForm]
	,
	"\
\\begin{name}
    \\text{body}
\\end{name}"
	,
	TestID -> "TeX conversion: name not in List"
]

Test[
	ToString[
		TeXEnvironment[{"name", {{a -> b, c, d -> e, f}, g}}, "body"],
		TeXForm
	]
	,
	"\
\\begin{name}[a=b,c,d=e,f]{g}
    \\text{body}
\\end{name}"
	,
	TestID -> "TeX conversion: command args"
]

Test[
	ToString[TeXEnvironment[{"name"}, "str", sym], TeXForm]
	,
	"\
\\begin{name}
    \\text{str}
    \\text{sym}
\\end{name}"
	,
	TestID -> "TeX conversion: 2 body expressions"
]


(* ::Subsubsection:: *)
(*Nested*)


Test[
	ToString[
		TeXEnvironment[{"env1"},
			TeXEnvironment[{"env2"}, "body"]
		]
		,
		TeXForm
	]
	,
	"\
\\begin{env1}
    \\begin{env2}
        \\text{body}
    \\end{env2}
\\end{env1}"
	,
	TestID -> "TeX conversion: nested: 2 levels"
]
Test[
	ToString[
		TeXEnvironment[{"env1"},
			"expr1"
			,
			TeXEnvironment[{"env2"},
				TeXEnvironment[{"env3"}, "expr2"],
				"expr3"
			]
			,
			"expr4"
		]
		,
		TeXForm
	]
	,
	"\
\\begin{env1}
    \\text{expr1}
    \\begin{env2}
        \\begin{env3}
            \\text{expr2}
        \\end{env3}
        \\text{expr3}
    \\end{env2}
    \\text{expr4}
\\end{env1}"
	,
	TestID -> "TeX conversion: nested: 3 levels with other expressions"
]


(* ::Subsubsection:: *)
(*Options*)


Test[
	ToString[
		TeXEnvironment[
			{"name", {"$a$"}, "ArgumentConverter" -> ToString},
			"body"
		]
		,
		TeXForm
	]
	,
	"\
\\begin{name}{$a$}
    \\text{body}
\\end{name}"
	,
	TestID -> "TeX conversion: \"ArgumentConverter\" option: ToString"
]

Test[
	ToString[
		TeXEnvironment[
			{"name", {"a"}, "BodyConverter" -> ToString},
			"body"
		]
		,
		TeXForm
	]
	,
	"\
\\begin{name}{\\text{a}}
    body
\\end{name}"
	,
	TestID -> "TeX conversion: \"BodyConverter\" option: ToString"
]

Test[
	ToString[
		TeXEnvironment[
			{"name", "BodySeparator" -> " "},
			"expr1",
			"expr2",
			"expr3"
		]
		,
		TeXForm
	]
	,
	"\
\\begin{name}
    \\text{expr1} \\text{expr2} \\text{expr3}
\\end{name}"
	,
	TestID -> "TeX conversion: \"BodySeparator\" option: space"
]

Test[
	ToString[
		TeXEnvironment[
			{"name", "DelimSeparator" -> " "},
			"expr1",
			"expr2",
			"expr3"
		]
		,
		TeXForm
	]
	,
	"\
\\begin{name} \\text{expr1}
    \\text{expr2}
    \\text{expr3} \\end{name}"
	,
	TestID -> "TeX conversion: \"DelimSeparator\" option: space"
]

Test[
	ToString[
		TeXEnvironment[
			{"env1", "Indentation" -> "\t"},
			"expr1",
			TeXEnvironment[{"env2", "Indentation" -> "  "}, "expr2"],
			"expr3"
		]
		,
		TeXForm
	]
	,
	"\
\\begin{env1}
\t\\text{expr1}
\t\\begin{env2}
\t  \\text{expr2}
\t\\end{env2}
\t\\text{expr3}
\\end{env1}"
	,
	TestID -> "TeX conversion: \"Indentation\" option: \t, two spaces"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest@$ContextPath
