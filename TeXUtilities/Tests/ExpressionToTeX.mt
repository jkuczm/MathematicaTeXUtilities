(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


Begin["TestEnvironment`ExpressionToTeX`"];


Needs["TeXUtilities`"];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*No format values*)


Test[
	Convert`TeX`ExpressionToTeX[a],
	"a",
	TestID -> "No format values: one symbol"
];
Test[
	Convert`TeX`ExpressionToTeX[1 + a^2],
	"a^2+1",
	TestID -> "No format values: expression with symbol"
];
Test[
	Convert`TeX`ExpressionToTeX[a[a]],
	"a(a)",
	TestID -> "No format values: nested"
];


(* ::Subsection:: *)
(*1 TeXForm format value*)


Format[a, TeXForm] = TeXVerbatim["\\a"];


Test[
	Convert`TeX`ExpressionToTeX[a],
	"\\a",
	TestID -> "1 TeXForm format value: one symbol"
];
Test[
	Convert`TeX`ExpressionToTeX[1 + a^2],
	"\\a^2+1",
	TestID -> "1 TeXForm format value: expression with symbol"
];
Test[
	Convert`TeX`ExpressionToTeX[a[a]],
	"\\a(\\a)",
	TestID -> "1 TeXForm format value: nested"
];


ClearAll[a];


(* ::Subsection:: *)
(*2 TeXForm format values*)



Format[a[x_], TeXForm] := TeXVerbatim["\\command{" <> ToString[x] <> "}"];
Format[a, TeXForm] = TeXVerbatim["\\a"];


Test[
	Convert`TeX`ExpressionToTeX[a],
	"\\a",
	TestID -> "2 TeXForm format values: one symbol"
];
Test[
	Convert`TeX`ExpressionToTeX[1 + a^2],
	"\\a^2+1",
	TestID -> "2 TeXForm format values: expression with symbol"
];

Test[
	Convert`TeX`ExpressionToTeX[a[b]],
	"\\command{b}",
	TestID -> "2 TeXForm format values: one function"
];
Test[
	Convert`TeX`ExpressionToTeX[1 + a[b]^2],
	"\\command{b}^2+1",
	TestID -> "2 TeXForm format values: expression with function"
];


Test[
	Convert`TeX`ExpressionToTeX[a + a[b]^2],
	"\\a+\\command{b}^2",
	TestID ->
		"2 TeXForm format values: expression with symbol and function"
];

Test[
	Convert`TeX`ExpressionToTeX[a[a]],
	"\\command{a}",
	TestID -> "2 TeXForm format values: nested"
];


ClearAll[a];


(* ::Subsection:: *)
(*2 TeXForm format values, with nesting*)



Format[a[x_], TeXForm] :=
	TeXVerbatim["\\command{" <> ToString[x, TeXForm] <> "}"];
Format[a, TeXForm] = TeXVerbatim["\\a"];

Test[
	Convert`TeX`ExpressionToTeX[a[b]],
	"\\command{b}",
	TestID -> "2 TeXForm format values, with nesting: one function"
];

Test[
	Convert`TeX`ExpressionToTeX[a[a]],
	"\\command{\\a}",
	TestID -> "2 TeXForm format values, with nesting: nested"
];


ClearAll[a];


(* ::Subsection:: *)
(*2 TeXForm format values, one not TeXForm*)



Format[a[_], CForm] = "a[x] in C";
Format[a, TeXForm] = TeXVerbatim["\\a"];


Test[
	Convert`TeX`ExpressionToTeX[a],
	"\\a",
	TestID -> "2 TeXForm format values, one not TeXForm: one symbol"
];

Test[
	Convert`TeX`ExpressionToTeX[a[b]],
	"\\a(b)",
	TestID -> "2 TeXForm format values, one not TeXForm: one function"
];

Test[
	Convert`TeX`ExpressionToTeX[a[a]],
	"\\a(\\a)",
	TestID -> "2 TeXForm format values, one not TeXForm: nested"
];


ClearAll[a];


(* ::Subsection:: *)
(*2 symbols with TeXForm format values*)


Format[a[x_], TeXForm] := TeXVerbatim["\\command{" <> ToString[x] <> "}"];
Format[a, TeXForm] = TeXVerbatim["\\a"];
Format[b, TeXForm] = TeXVerbatim["{\\bf b}"];


Test[
	Convert`TeX`ExpressionToTeX[b + a[b]^2],
	"{\\bf b}+\\command{b}^2",
	TestID -> "2 symbols with TeXForm format values: expression with both"
];
Test[
	Convert`TeX`ExpressionToTeX[a[b]],
	"\\command{b}",
	TestID -> "2 symbols with TeXForm format values: nested (in function)"
];
Test[
	Convert`TeX`ExpressionToTeX[b[a]],
	"{\\bf b}(\\a)",
	TestID -> "2 symbols with TeXForm format values: nested (in symbol)"
];


ClearAll[a, b];


(* ::Subsection:: *)
(*Complicated*)


Format[h[x__], TeXForm] :=
	TeXVerbatim["\\h" <> ("{" <> ToString[#, TeXForm] <> "}" & /@ {x})];
Format[myEnv[x__], TeXForm] := 
	TeXVerbatim[
		"\\begin{myEnv}" <>
		("\n    " <> ToString[#, TeXForm] <> "\\\\" & /@ {x}) <>
		"\n\\end{myEnv}"
	];


Test[
	Convert`TeX`ExpressionToTeX[myEnv[1 + alpha, 5 - h[3, h[2]]]]
	,
	"\
\\begin{myEnv}
    \\alpha +1\\\\
    5-\\h{3}{\\h{2}}\\\\
\\end{myEnv}"
	,
	TestID -> "Complicated"
];


ClearAll[h, myEnv];


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"];
Quiet[Remove["`*"], {Remove::rmnsm}];


End[];
