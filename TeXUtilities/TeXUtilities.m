(* ::Package:: *)

BeginPackage@"TeXUtilities`"


Unprotect@"`*"
ClearAll@"`*"


(* ::Section:: *)
(*Usage messages*)


TeXVerbatim::usage =
"\
TeXVerbatim[\"str\"] \
converted to TeX gives verbatim \"str\"."


TeXDelimited::usage =
"\
TeXDelimited[\"startDelim\", expr1, expr2, ..., \"endDelim\"] \
converted to TeX gives:
startDelim
	expr1
	expr2
	...
endDelim
with expri converted to TeXForm."


TeXCommand::usage =
"\
TeXCommand[\"name\"] \
represents TeX command \"\\name\".\

TeXCommand[\"name\", {arg1, arg2, ...}] \
represents TeX command with arguments \"\\name{arg1}{arg2}...\".\

TeXCommand[\
\"name\", {\
{opt1 -> val1, opt2, opt3 -> val3, ...}, \
arg1, arg2, ...\
}] \
represents TeX command with optional arguments \
\"\\name[opt1=val1,opt2,opt3=val3,...]{arg1}{arg2}...\"

Arguments argi, optional arguments opt1 and their values vali are converted \
to TeX using value of \"ArgumentConverter\" option."


TeXEnvironment::usage =
"\
TeXEnvironment[\"name\", expr1, expr2, ...] or \
TeXEnvironment[{\"name\"}, expr1, expr2, ...] \
represents TeX environment:
\\begin{name}
	expr1
	expr2
	...
\\end{name}
with expri converted to TeXForm.\

TeXEnvironment[\
{\"name\", {{opt1 -> val1, opt2, opt3 -> val3, ...}, arg1, arg2, ...}}, \
expr1, expr2, ...\
] \
represents TeX environment:
\\begin{name}[opt1=val1,opt2,opt3=val3,...]{arg1}{arg2}...
	expr1
	expr2
	...
\\end{name}
with expri, argi, opti and vali converted to TeXForm."


(* ::Section:: *)
(*Implementation*)


Begin@"`Private`"


ClearAll@"`*"


(* ::Subsection:: *)
(*Private symbols*)


(* ::Subsubsection:: *)
(*heldToTeXString*)


heldToTeXString::usage =
"\
heldToTeXString[expr]\
returns string corresponding to TeX form of given expression expr. \
expr is not evaluated."


heldToTeXString =
	Function[Null, ToString[Unevaluated@#, TeXForm], HoldAllComplete]


(* ::Subsubsection:: *)
(*heldOptionQ*)


heldOptionQ::usage =
"\
heldOptionQ[e]\
returns True if e can be considered an option or list of options, and False \
otherwise. e is not evaluated."


heldOptionQ = Function[Null, OptionQ@Unevaluated@#, HoldAllComplete]


(* ::Subsubsection:: *)
(*teXCommandArgument*)


teXCommandArgument::usage =
"\
teXCommandArgument[argConverter][arg]\
returns string representing argument of TeX command i.e. arg convetred to TeX \
using argConverter, wrapped in curly bracket.\

teXCommandArgument[argConverter][{opt1, opt2 -> val2, ...}]\
returns string representing list of optional arguments of TeX command i.e. \
\"[opt1,opt2=val2,...]\" with opti and vali converted to TeX using \
argConverter."


teXCommandArgument = Function[argConverter, Function[Null,
	If[ListQ@Unevaluated@#,
		StringJoin["[",
			Riffle[
				Replace[Unevaluated@#,
					{
						(Rule | RuleDelayed)[argName_, value_] :>
							argConverter@argName <> "=" <> argConverter@value,
						arg_ :> argConverter@arg
					},
					{1}
				],
				","
			],
		"]"]
	(* else *),
		"{" <> argConverter@# <> "}"
	],
	HoldAllComplete
]]


(* ::Subsubsection:: *)
(*teXRelevantQ*)


teXRelevantQ::usage =
"\
teXRelevantQ[sym] \
returns True if given symbol sym has TeXForm format values and is not Locked, \
returns False otherwise."


teXRelevantQ = Function[Null,
	Not@MemberQ[Attributes@Unevaluated@#, Locked] &&
		MemberQ[FormatValues@Unevaluated@#,
			_[lhs_ /; Not@FreeQ[lhs, HoldPattern@Format[_, TeXForm]] , _]
		],
	HoldAllComplete
]


(* ::Subsubsection:: *)
(*getTeXRelevantSymbols*)


getTeXRelevantSymbols::usage =
"\
getTeXRelevantSymbols[expr] \
returns HoldComplete[sym1, sym2, ...] where symi are non-Locked symbols with \
TeXForm format values."


getTeXRelevantSymbols = Function[Null,
	HoldComplete @@ Union @@ Cases[
		Unevaluated@#,
		s : Except[HoldPattern@Symbol@___, _Symbol]?teXRelevantQ :>
			HoldComplete@s
		,
		{0, Infinity},
		Heads -> True
	],
	HoldAllComplete
]


(* ::Subsubsection:: *)
(*teXToTraditionalFormat*)


teXToTraditionalFormat::usage =
"\
teXToTraditionalFormat[sym] \
replaces TeXForm format values, of given symbol sym, with TraditionalForm \
format values."


teXToTraditionalFormat = Function[Null,
	FormatValues@Unevaluated@# = Replace[FormatValues@Unevaluated@#,
		h_[lhs_ /; Not@FreeQ[lhs, HoldPattern@Format[_, TeXForm]], rhs_] :> h[
			lhs /. HoldPattern@Format[x_, TeXForm] :>
				MakeBoxes[x, TraditionalForm],
			Format[rhs, TraditionalForm]
		],
		{1}
	],
	HoldAllComplete
]


(* ::Subsubsection:: *)
(*expressionToTeX*)


expressionToTeX::usage =
"\
expressionToTeX[arg1, arg2, ...] \
returns same result as Convert`TeX`ExpressionToTeX[arg1, arg2, ...] with down \
values not modified by this package."


expressionToTeX // Attributes = HoldAllComplete


(* ::Subsubsection:: *)
(*$expressionToTeXDV*)


$expressionToTeXDV::usage =
"\
$expressionToTeXDV \
is a down value for Convert`TeX`ExpressionToTeX, calling original \
Convert`TeX`ExpressionToTeX down value, except this one, in environment with \
TeXForm format values, of symbols from first argument, locally replaced with \
TraditionalForm format values."


$expressionToTeXDV = HoldPattern@Convert`TeX`ExpressionToTeX[expr_, rest___] :>
	Replace[getTeXRelevantSymbols@expr, HoldComplete@syms___ :>
		Internal`InheritedBlock[{expressionToTeX, syms},
			Unprotect@{syms};
			Scan[teXToTraditionalFormat, Unevaluated@{syms}];
			expressionToTeX // DownValues = DeleteCases[
				DownValues@Convert`TeX`ExpressionToTeX,
				Verbatim@$expressionToTeXDV
			] /. Convert`TeX`ExpressionToTeX -> expressionToTeX;
			expressionToTeX[expr, rest]
		]
	]


(* ::Subsection:: *)
(*ExpressionToTeX patch*)


(* Evaluate symbol to load necessary contexts. *)
Convert`TeX`ExpressionToTeX


With[
	{
		protected = Unprotect@Convert`TeX`ExpressionToTeX,
		dv = DownValues@Convert`TeX`ExpressionToTeX
	},
	If[dv === {} || First@dv =!= $expressionToTeXDV,
		Convert`TeX`ExpressionToTeX // DownValues = Prepend[
			DeleteCases[dv, Verbatim@$expressionToTeXDV],
			$expressionToTeXDV
		]
	];
	Protect@protected
]


(* ::Subsection:: *)
(*Public symbols*)


(* ::Subsubsection:: *)
(*TeXVerbatim*)


TeXVerbatim[arg : Except@_String] := (
	Message[TeXVerbatim::string, HoldForm@1, HoldForm@TeXVerbatim@arg];
	$Failed
)

TeXVerbatim@args___ := With[{argsNo = Length@HoldComplete@args},
	(
		Message[TeXVerbatim::argx, HoldForm@TeXVerbatim, HoldForm@argsNo];
		$Failed
	) /; argsNo =!= 1
]


System`Convert`TeXFormDump`maketex@RowBox@{
	"TeXVerbatim",
	"(" | "[",
	arg_String?(StringMatchQ[#, "\"" ~~ ___ ~~ "\""]&),
	")" | "]"
} :=
	ToExpression@arg


(* ::Subsubsection:: *)
(*TeXDelimited*)


TeXDelimited // Options = {
	"BodyConverter" -> heldToTeXString,
	"BodySeparator" -> "\n",
	"DelimSeparator" -> "\n",
	"Indentation" -> "    "
}


TeXDelimited[arg : Repeated[_, {0, 1}]] := (
	Message[TeXDelimited::argm,
		HoldForm@TeXDelimited,
		HoldForm@Evaluate@Length@HoldComplete@arg,
		HoldForm@2
	];
	$Failed
)

TeXDelimited[start : Except@_String, rest__] := (
	Message[TeXDelimited::string,
		HoldForm@1, HoldForm@TeXDelimited[start, rest]
	];
	$Failed
)

TeXDelimited[
	most__,
	end : Except[_String | _?heldOptionQ],
	opts : Longest@OptionsPattern[]
] := (
	Message[TeXDelimited::string,
		HoldForm@Evaluate[Length@HoldComplete@most + 1],
		HoldForm@TeXDelimited[most, end, opts]
	];
	$Failed
)


TeXDelimited /: MakeBoxes[
	TeXDelimited[start_String, body___, end_String, opts : OptionsPattern[]],
	TraditionalForm
] := Module[{bodyConv, bodySep, delSep, indent},
	{bodyConv, bodySep, delSep, indent} = OptionValue[TeXDelimited, {opts},
		{"BodyConverter", "BodySeparator", "DelimSeparator", "Indentation"}
	];
	ToBoxes[
		TeXVerbatim@StringJoin[
			start,
			StringReplace[
				StringJoin[
					If[Length@HoldComplete@body > 0, delSep, ""],
					Riffle[bodyConv /@ Unevaluated@{body}, bodySep]
				],
				"\n" -> "\n" <> indent
			],
			delSep,
			end
		]
		,
		TraditionalForm
	]
]


(* ::Subsubsection:: *)
(*TeXCommand*)


TeXCommand // Options = {"ArgumentConverter" -> heldToTeXString}


TeXCommand[] := (
	Message[TeXCommand::argm, HoldForm@TeXCommand, HoldForm@0, HoldForm@1];
	$Failed
)

TeXCommand[name : Except@_String, rest___] := (
	Message[TeXCommand::string, HoldForm@1, HoldForm@TeXCommand[name, rest]];
	$Failed
)

TeXCommand[name_, args : Except@_List, rest___] := (
	Message[TeXCommand::list,
		HoldForm@TeXCommand[name, args, rest], HoldForm@2
	];
	$Failed
)


TeXCommand /: MakeBoxes[
	TeXCommand[name_String, args_List : {}, opts : OptionsPattern[]],
	TraditionalForm
] := ToBoxes[
	TeXVerbatim@StringJoin["\\", name,
		teXCommandArgument@OptionValue[
			TeXCommand, {opts}, "ArgumentConverter"
		] /@ Unevaluated@args
	],
	TraditionalForm
]


(* ::Subsubsection:: *)
(*TeXEnvironment*)


TeXEnvironment::StrOrListWithStr =
"String or List with first element being String expected at position 1 in `1`."


TeXEnvironment // Options = Join[Options@TeXCommand, Options@TeXDelimited]


TeXEnvironment[] := (
	Message[TeXEnvironment::argm,
		HoldForm@TeXEnvironment, HoldForm@0, HoldForm@1
	];
	$Failed
)

TeXEnvironment[name : Except[_String | {_String, ___}], rest___] := (
	Message[TeXEnvironment::StrOrListWithStr,
		HoldForm@TeXEnvironment[name, rest]
	];
	$Failed
)


TeXEnvironment /: MakeBoxes[
	TeXEnvironment[
		{name_String, Repeated[{args___}, {0, 1}], opts : OptionsPattern[]} |
			name_String,
		body___
	]
	,
	TraditionalForm
] := MakeBoxes[TeXDelimited[#1, body, #2, #3], TraditionalForm]&[
	StringJoin[
		"\\begin{", name, "}",
		teXCommandArgument@OptionValue[
			TeXEnvironment, {opts}, "ArgumentConverter"
		] /@ Unevaluated@{args}
	],
	"\\end{" <> name <> "}",
	FilterRules[{opts, Options@TeXEnvironment}, Options@TeXDelimited]
]


End[]


Protect@"`*"


EndPackage[]
