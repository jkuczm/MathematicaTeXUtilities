(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


Begin["TestEnvironment`ApplyTeXFormFormat`"];


AppendTo[$ContextPath, "FormatTeXFormPatch`Private`"];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Wrong argument patterns*)


TestMatch[
	ApplyTeXFormFormat[],
	HoldPattern[ApplyTeXFormFormat[]],
	TestID -> "no args"
];
TestMatch[
	ApplyTeXFormFormat[a, b],
	HoldPattern[ApplyTeXFormFormat[a, b]],
	TestID -> "two args"
];


(* ::Subsection:: *)
(*No format values*)


Block[
	{FormatValues}
	,
	FormatValues[a] = {};
	
	Test[
		ApplyTeXFormFormat[a],
		a,
		TestID -> "No format values: one symbol"
	];
	Test[
		ApplyTeXFormFormat[1 + a^2],
		1 + a^2,
		TestID -> "No format values: expression with symbol"
	];
	Test[
		ApplyTeXFormFormat[a[a]],
		a[a],
		TestID -> "No format values: nested"
	];
];


(* ::Subsection:: *)
(*1 TeXForm format value*)


Block[
	{FormatValues}
	,
	FormatValues[a] = {
		HoldPattern[Format[a, TeXForm]] :> aInTeX
	};
	
	
	Test[
		ApplyTeXFormFormat[a],
		aInTeX,
		TestID -> "1 TeXForm format value: one symbol"
	];
	Test[
		ApplyTeXFormFormat[1 + a^2],
		1 + aInTeX^2,
		TestID -> "1 TeXForm format value: expression with symbol"
	];
	Test[
		ApplyTeXFormFormat[a[a]],
		aInTeX[aInTeX],
		TestID -> "1 TeXForm format value: nested"
	];
];


(* ::Subsection:: *)
(*2 TeXForm format values*)


Block[
	{FormatValues}
	,
	FormatValues[a] = {
		HoldPattern[Format[a[x_], TeXForm]] :> f[2 x],
		HoldPattern[Format[a, TeXForm]] :> aInTeX
	};
	
	
	Test[
		ApplyTeXFormFormat[a],
		aInTeX,
		TestID -> "2 TeXForm format values: one symbol"
	];
	Test[
		ApplyTeXFormFormat[1 + a^2],
		1 + aInTeX^2,
		TestID -> "2 TeXForm format values: expression with symbol"
	];
	
	Test[
		ApplyTeXFormFormat[a[b]],
		f[2 b],
		TestID -> "2 TeXForm format values: one function"
	];
	Test[
		ApplyTeXFormFormat[1 + a[b]^2],
		1 + f[2 b]^2,
		TestID -> "2 TeXForm format values: expression with function"
	];
	
	
	Test[
		ApplyTeXFormFormat[a + a[b]^2],
		aInTeX + f[2 b]^2,
		TestID ->
			"2 TeXForm format values: expression with symbol and function"
	];
	
	Test[
		ApplyTeXFormFormat[a[a]],
		f[2 a],
		TestID -> "2 TeXForm format values: nested"
	];
];


(* ::Subsection:: *)
(*2 TeXForm format values, with nesting*)


Block[
	{FormatValues}
	,
	FormatValues[a] = {
		HoldPattern[Format[a[x_], TeXForm]] :> f[2 ApplyTeXFormFormat[x]],
		HoldPattern[Format[a, TeXForm]] :> aInTeX
	};
	
	Test[
		ApplyTeXFormFormat[a[b]],
		f[2 b],
		TestID -> "2 TeXForm format values, with nesting: one function"
	];
	
	Test[
		ApplyTeXFormFormat[a[a]],
		f[2 aInTeX],
		TestID -> "2 TeXForm format values, with nesting: nested"
	];
];


(* ::Subsection:: *)
(*2 TeXForm format values, one not TeXForm*)


Block[
	{FormatValues}
	,
	FormatValues[a] = {
		HoldPattern[Format[a[x_], CForm]] :> f[2 x],
		HoldPattern[Format[a, TeXForm]] :> aInTeX
	};
	
	
	Test[
		ApplyTeXFormFormat[a],
		aInTeX,
		TestID -> "2 TeXForm format values, one not TeXForm: one symbol"
	];
	
	Test[
		ApplyTeXFormFormat[a[b]],
		aInTeX[b],
		TestID -> "2 TeXForm format values, one not TeXForm: one function"
	];
	
	Test[
		ApplyTeXFormFormat[a[a]],
		aInTeX[aInTeX],
		TestID -> "2 TeXForm format values, one not TeXForm: nested"
	];
];


(* ::Subsection:: *)
(*2 symbols with TeXForm format values*)


Block[
	{FormatValues}
	,
	FormatValues[a] = {
		HoldPattern[Format[a[x_], TeXForm]] :> f[2 x],
		HoldPattern[Format[a, TeXForm]] :> aInTeX
	};
	FormatValues[b] = {
		HoldPattern[Format[b, TeXForm]] :> bInTeX
	};
	
	
	Test[
		ApplyTeXFormFormat[b + a[b]^2],
		bInTeX + f[2 b]^2,
		TestID -> "2 symbols with TeXForm format values: expression with both"
	];
	Test[
		ApplyTeXFormFormat[a[b]],
		f[2 b],
		TestID -> "2 symbols with TeXForm format values: nested (in function)"
	];
	Test[
		ApplyTeXFormFormat[b[a]],
		bInTeX[aInTeX],
		TestID -> "2 symbols with TeXForm format values: nested (in symbol)"
	];
];


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"];
Quiet[Remove["`*"], {Remove::rmnsm}];


End[];
