(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["TeXUtilities`Tests`applyTeXFormFormat`", {"MUnit`"}]


<<TeXUtilities`FormatTeXFormPatch`
AppendTo[$ContextPath, "TeXUtilities`FormatTeXFormPatch`Private`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Wrong argument patterns*)


Test[
	applyTeXFormFormat[] // HoldComplete@#&,
	applyTeXFormFormat[] // HoldComplete,
	TestID -> "no args"
]
Test[
	applyTeXFormFormat[a, b] // HoldComplete@#&,
	applyTeXFormFormat[a, b] // HoldComplete,
	TestID -> "two args"
]


(* ::Subsection:: *)
(*No format values*)


Module[{a},
	FormatValues@a = {};
	
	Test[
		applyTeXFormFormat@a,
		a,
		TestID -> "No format values: one symbol"
	];
	Test[
		applyTeXFormFormat[1 + a^2],
		1 + a^2,
		TestID -> "No format values: expression with symbol"
	];
	Test[
		applyTeXFormFormat@a@a,
		a@a,
		TestID -> "No format values: nested"
	]
]


(* ::Subsection:: *)
(*1 TeXForm format value*)


Module[{a, aInTeX},
	FormatValues@a = {HoldPattern@Format[a, TeXForm] :> aInTeX};
	
	Test[
		applyTeXFormFormat@a,
		aInTeX,
		TestID -> "1 TeXForm format value: one symbol"
	];
	Test[
		applyTeXFormFormat[1 + a^2],
		1 + aInTeX^2,
		TestID -> "1 TeXForm format value: expression with symbol"
	];
	Test[
		applyTeXFormFormat@a@a,
		aInTeX@aInTeX,
		TestID -> "1 TeXForm format value: nested"
	]
]


(* ::Subsection:: *)
(*2 TeXForm format values*)


Module[{a, aInTeX, b},
	FormatValues@a = {
		HoldPattern@Format[a@x_, TeXForm] :> f[2 x],
		HoldPattern@Format[a, TeXForm] :> aInTeX
	};
	
	Test[
		applyTeXFormFormat@a,
		aInTeX,
		TestID -> "2 TeXForm format values: one symbol"
	];
	Test[
		applyTeXFormFormat[1 + a^2],
		1 + aInTeX^2,
		TestID -> "2 TeXForm format values: expression with symbol"
	];
	
	Test[
		applyTeXFormFormat@a@b,
		f[2 b],
		TestID -> "2 TeXForm format values: one function"
	];
	Test[
		applyTeXFormFormat[1 + a[b]^2],
		1 + f[2 b]^2,
		TestID -> "2 TeXForm format values: expression with function"
	];
	
	Test[
		applyTeXFormFormat[a + a[b]^2],
		aInTeX + f[2 b]^2,
		TestID ->
			"2 TeXForm format values: expression with symbol and function"
	];
	Test[
		applyTeXFormFormat@a@a,
		f[2 a],
		TestID -> "2 TeXForm format values: nested"
	]
]


(* ::Subsection:: *)
(*2 TeXForm format values, with nesting*)


Module[{a, aInTeX, b},
	FormatValues@a = {
		HoldPattern@Format[a[x_], TeXForm] :> f[2 applyTeXFormFormat@x],
		HoldPattern@Format[a, TeXForm] :> aInTeX
	};
	
	Test[
		applyTeXFormFormat@a@b,
		f[2 b],
		TestID -> "2 TeXForm format values, with nesting: one function"
	];
	Test[
		applyTeXFormFormat@a@a,
		f[2 aInTeX],
		TestID -> "2 TeXForm format values, with nesting: nested"
	]
]


(* ::Subsection:: *)
(*2 TeXForm format values, one not TeXForm*)


Module[{a, aInTeX, b},
	FormatValues@a = {
		HoldPattern@Format[a[x_], CForm] :> f[2 x],
		HoldPattern@Format[a, TeXForm] :> aInTeX
	};
	
	Test[
		applyTeXFormFormat@a,
		aInTeX,
		TestID -> "2 TeXForm format values, one not TeXForm: one symbol"
	];
	Test[
		applyTeXFormFormat@a@b,
		aInTeX@b,
		TestID -> "2 TeXForm format values, one not TeXForm: one function"
	];
	Test[
		applyTeXFormFormat@a@a,
		aInTeX@aInTeX,
		TestID -> "2 TeXForm format values, one not TeXForm: nested"
	]
]


(* ::Subsection:: *)
(*2 symbols with TeXForm format values*)


Module[{a, aInTeX, b, bInTeX},
	FormatValues@a = {
		HoldPattern@Format[a@x_, TeXForm] :> f[2 x],
		HoldPattern@Format[a, TeXForm] :> aInTeX
	};
	FormatValues@b = {
		HoldPattern@Format[b, TeXForm] :> bInTeX
	};
	
	Test[
		applyTeXFormFormat[b + a[b]^2],
		bInTeX + f[2 b]^2,
		TestID -> "2 symbols with TeXForm format values: expression with both"
	];
	Test[
		applyTeXFormFormat@a@b,
		f[2 b],
		TestID -> "2 symbols with TeXForm format values: nested (in function)"
	];
	Test[
		applyTeXFormFormat@b@a,
		bInTeX@aInTeX,
		TestID -> "2 symbols with TeXForm format values: nested (in symbol)"
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest@$ContextPath
