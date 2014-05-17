(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


Begin["TestEnvironment`TeXCommandArgument`"];


Get["TeXUtilities`TeXUtilities`"];
AppendTo[$ContextPath, "TeXUtilities`Private`"];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Wrong argument patterns*)


TestMatch[
	TeXCommandArgument[],
	HoldPattern[TeXCommandArgument[]],
	TestID -> "no args"
];
TestMatch[
	TeXCommandArgument[a],
	HoldPattern[TeXCommandArgument[a]],
	TestID -> "one arg"
];
TestMatch[
	TeXCommandArgument[a, b, c],
	HoldPattern[TeXCommandArgument[a, b, c]],
	TestID -> "three args"
];


(* ::Subsection:: *)
(*Non-List command argument*)


Test[
	TeXCommandArgument["str", ToString],
	"{str}",
	TestID -> "two args: String, ToString"
];
Test[
	TeXCommandArgument[a, ToString],
	"{a}",
	TestID -> "two args: Symbol, ToString"
];


(* ::Subsection:: *)
(*List command argument*)


Test[
	TeXCommandArgument[{}, ToString],
	"[]",
	TestID -> "two args: empty List, ToString"
];
Test[
	TeXCommandArgument[{a, b, c}, ToString],
	"[a,b,c]",
	TestID -> "two args: List of symbols, ToString"
];
Test[
	TeXCommandArgument[{a -> b, c -> d}, ToString],
	"[a=b,c=d]",
	TestID -> "two args: List of rules, ToString"
];
Test[
	TeXCommandArgument[{a -> b, c, d -> e, f}, ToString],
	"[a=b,c,d=e,f]",
	TestID -> "two args: List of rules and symbols, ToString"
];


(* ::Subsection:: *)
(*List command argument*)


Test[
	TeXCommandArgument[a, ToString[f[#]]&],
	"{f[a]}",
	TestID -> "two args: Symbol, custom function"
];
Test[
	TeXCommandArgument[{a -> b, c, d -> e, f}, ToString[f[#]]&],
	"[f[a]=f[b],f[c],f[d]=f[e],f[f]]",
	TestID -> "two args: List of rules and symbols, custom function"
];


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"];
Quiet[Remove["`*"], {Remove::rmnsm}];


End[];
