(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["TeXUtilities`Tests`teXCommandArgument`", {"MUnit`"}]


<<TeXUtilities`TeXUtilities`
AppendTo[$ContextPath, "TeXUtilities`Private`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Wrong argument patterns*)


Test[
	teXCommandArgument[] // HoldComplete@#&,
	teXCommandArgument[] // HoldComplete,
	TestID -> "no args"
]
Test[
	teXCommandArgument@a // HoldComplete@#&,
	teXCommandArgument@a // HoldComplete,
	TestID -> "one arg"
]
Test[
	teXCommandArgument[a, b, c] // HoldComplete@#&,
	teXCommandArgument[a, b, c] // HoldComplete,
	TestID -> "three args"
]


(* ::Subsection:: *)
(*Non-List command argument*)


Test[
	teXCommandArgument["str", ToString],
	"{str}",
	TestID -> "two args: String, ToString"
]
Test[
	teXCommandArgument[a, ToString],
	"{a}",
	TestID -> "two args: Symbol, ToString"
]


(* ::Subsection:: *)
(*List command argument*)


Test[
	teXCommandArgument[{}, ToString],
	"[]",
	TestID -> "two args: empty List, ToString"
]
Test[
	teXCommandArgument[{a, b, c}, ToString],
	"[a,b,c]",
	TestID -> "two args: List of symbols, ToString"
]
Test[
	teXCommandArgument[{a -> b, c -> d}, ToString],
	"[a=b,c=d]",
	TestID -> "two args: List of rules, ToString"
]
Test[
	teXCommandArgument[{a -> b, c, d -> e, f}, ToString],
	"[a=b,c,d=e,f]",
	TestID -> "two args: List of rules and symbols, ToString"
]


(* ::Subsection:: *)
(*List command argument*)


Test[
	teXCommandArgument[a, ToString@f@#&],
	"{f[a]}",
	TestID -> "two args: Symbol, custom function"
]
Test[
	teXCommandArgument[
		{a -> b, c, d -> e, f}, ToString@f@#&
	],
	"[f[a]=f[b],f[c],f[d]=f[e],f[f]]",
	TestID -> "two args: List of rules and symbols, custom function"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest@$ContextPath
