(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["TeXUtilities`Tests`applyTeXFormFormat`", {"MUnit`"}]


<<TeXUtilities`FormatTeXFormPatch`
AppendTo[$ContextPath, "TeXUtilities`FormatTeXFormPatch`Private`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*No format values*)


Module[{a},
	FormatValues@a = {};
	
	Test[
		applyTeXFormFormat@a,
		HoldComplete@a,
		TestID -> "No format values: one symbol"
	];
	Test[
		applyTeXFormFormat[1 + a^2],
		HoldComplete[1 + a^2],
		TestID -> "No format values: expression with symbol"
	];
	Test[
		applyTeXFormFormat@a@a,
		HoldComplete@a@a,
		TestID -> "No format values: nested"
	]
]


(* ::Subsection:: *)
(*1 TeXForm format value*)


Module[{a, aInTeX},
	FormatValues@a = {HoldPattern@Format[a, TeXForm] :> aInTeX};
	
	Test[
		applyTeXFormFormat@a,
		HoldComplete@aInTeX,
		TestID -> "1 TeXForm format value: one symbol"
	];
	Test[
		applyTeXFormFormat[1 + a^2],
		HoldComplete[1 + aInTeX^2],
		TestID -> "1 TeXForm format value: expression with symbol"
	];
	Test[
		applyTeXFormFormat@a@a,
		HoldComplete@aInTeX@aInTeX,
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
		HoldComplete@aInTeX,
		TestID -> "2 TeXForm format values: one symbol"
	];
	Test[
		applyTeXFormFormat[1 + a^2],
		HoldComplete[1 + aInTeX^2],
		TestID -> "2 TeXForm format values: expression with symbol"
	];
	
	Test[
		applyTeXFormFormat@a@b,
		HoldComplete@f[2 b],
		TestID -> "2 TeXForm format values: one function"
	];
	Test[
		applyTeXFormFormat[1 + a[b]^2],
		HoldComplete[1 + f[2 b]^2],
		TestID -> "2 TeXForm format values: expression with function"
	];
	
	Test[
		applyTeXFormFormat[a + a[b]^2],
		HoldComplete[aInTeX + f[2 b]^2],
		TestID ->
			"2 TeXForm format values: expression with symbol and function"
	];
	Test[
		applyTeXFormFormat@a@a,
		HoldComplete@f[2 a],
		TestID -> "2 TeXForm format values: nested"
	]
]


(* ::Subsection:: *)
(*2 TeXForm format values, with nesting*)


Module[{a, aInTeX, b},
	FormatValues@a = {
		HoldPattern@Format[a[x_], TeXForm] :> f[2 First@applyTeXFormFormat@x],
		HoldPattern@Format[a, TeXForm] :> aInTeX
	};
	
	Test[
		applyTeXFormFormat@a@b,
		HoldComplete@f[2 b],
		TestID -> "2 TeXForm format values, with nesting: one function"
	];
	Test[
		applyTeXFormFormat@a@a,
		HoldComplete@f[2 aInTeX],
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
		HoldComplete@aInTeX,
		TestID -> "2 TeXForm format values, one not TeXForm: one symbol"
	];
	Test[
		applyTeXFormFormat@a@b,
		HoldComplete@aInTeX@b,
		TestID -> "2 TeXForm format values, one not TeXForm: one function"
	];
	Test[
		applyTeXFormFormat@a@a,
		HoldComplete@aInTeX@aInTeX,
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
		HoldComplete[bInTeX + f[2 b]^2],
		TestID -> "2 symbols with TeXForm format values: expression with both"
	];
	Test[
		applyTeXFormFormat@a@b,
		HoldComplete@f[2 b],
		TestID -> "2 symbols with TeXForm format values: nested (in function)"
	];
	Test[
		applyTeXFormFormat@b@a,
		HoldComplete@bInTeX@aInTeX,
		TestID -> "2 symbols with TeXForm format values: nested (in symbol)"
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest@$ContextPath
