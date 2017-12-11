(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["TeXUtilities`Tests`teXCommandArgument`", {"MUnit`"}]


<<TeXUtilities`
AppendTo[$ContextPath, "TeXUtilities`Private`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Non-List command argument*)


Test[
	teXCommandArgument[ToString]@"str",
	"{str}",
	TestID -> "String"
]
Test[
	teXCommandArgument[ToString]@a,
	"{a}",
	TestID -> "Symbol"
]

Test[
	teXCommandArgument[ToString@f@#&]@a,
	"{f[a]}",
	TestID -> "Custom function: Symbol"
]

Block[{leaked = False},
	Test[
		teXCommandArgument[Function[Null, ToString@Unevaluated@#, HoldFirst]][
			leaked = True
		],
		"{leaked = True}",
		TestID -> "held ToString: evaluation leak"
	];
	Test[
		leaked,
		False,
		TestID -> "held ToString: evaluation leak: leak"
	]
]


(* ::Subsection:: *)
(*List command argument*)


Test[
	teXCommandArgument[ToString]@{},
	"[]",
	TestID -> "empty List"
]
Test[
	teXCommandArgument[ToString]@{a, b, c},
	"[a,b,c]",
	TestID -> "List of symbols"
]
Test[
	teXCommandArgument[ToString]@{a -> b, c -> d},
	"[a=b,c=d]",
	TestID -> "List of rules"
]
Test[
	teXCommandArgument[ToString]@{a -> b, c, d -> e, f},
	"[a=b,c,d=e,f]",
	TestID -> "List of rules and symbols"
]

Test[
	teXCommandArgument[ToString@f@#&]@{a -> b, c, d -> e, f},
	"[f[a]=f[b],f[c],f[d]=f[e],f[f]]",
	TestID -> "Custom function: List of rules and symbols"
]

Block[{leaked = False, leakedLHS = False, leakedRHS = False},
	Test[
		teXCommandArgument[Function[Null, ToString@Unevaluated@#, HoldFirst]]@
			{leaked = True, (leakedLHS = True) -> (leakedRHS = True)},
		"[leaked = True,leakedLHS = True=leakedRHS = True]",
		TestID -> "held ToString: List evaluation leak"
	];
	Test[
		leaked,
		False,
		TestID -> "held ToString: List evaluation leak: non-rule arg leak"
	];
	Test[
		leakedLHS,
		False,
		TestID -> "held ToString: List evaluation leak: rule arg LHS leak"
	];
	Test[
		leakedRHS,
		False,
		TestID -> "held ToString: List evaluation leak: rule arg RHS leak"
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest@$ContextPath
