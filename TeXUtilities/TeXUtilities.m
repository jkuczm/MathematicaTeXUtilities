(* ::Package:: *)

BeginPackage["TeXUtilities`"]


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


(*
	Unprotect all symbols in this context
	(all public symbols provided by this package)
*)
Unprotect["`*"]


Begin["`Private`"]


(* ::Subsection:: *)
(*Private symbols usage*)


TeXCommandArgument::usage =
"\
TeXCommandArgument[arg, argConverter]\
returns string representing argument of TeX command i.e. arg convetred to TeX \
using argConverter, wrapped in curly bracket.\

TeXCommandArgument[{opt1, opt2 -> val2, ...}, argConverter]\
returns string representing list of optional arguments of TeX command i.e. \
\"[opt1,opt2=val2,...]\" with opti and vali converted to TeX using \
argConverter."


(* ::Subsection:: *)
(*TeXVerbatim*)


TeXVerbatim[arg:Except[_String]] := (
	Message[TeXVerbatim::string, 1, HoldForm[TeXVerbatim[arg]]];
	$Failed
)

TeXVerbatim[args___] :=
	With[
		{argsNo = Length[{args}]}
		,
		(
			Message[
				TeXVerbatim::argx,
				HoldForm[TeXVerbatim],
				HoldForm[argsNo]
			];
			$Failed
		)
			/; argsNo =!= 1
	]


System`Convert`TeXFormDump`maketex[
	RowBox[{
		"TeXVerbatim",
		"(" | "[", 
		arg_String?(StringMatchQ[#, "\"" ~~ ___ ~~ "\""] &), 
		")" | "]"
	}]] :=
		ToExpression[arg]


(* ::Subsection:: *)
(*TeXDelimited*)


Options[TeXDelimited] = {
	"BodyConverter" -> (ToString[#, TeXForm]&),
	"BodySeparator" -> "\n",
	"DelimSeparator" -> "\n",
	"Indentation" -> "    "
}


TeXDelimited[arg:Repeated[_, {0, 1}]] := (
	Message[TeXDelimited::argm, HoldForm[TeXDelimited], Length[{arg}], 2];
	$Failed
)

TeXDelimited[start:Except[_String], rest__] := (
	Message[TeXDelimited::string, 1, HoldForm[TeXDelimited[start, rest]]];
	$Failed
)

TeXDelimited[
	rest__,
	end:Except[_String | _?OptionQ],
	opts:Longest[OptionsPattern[]]
] := (
	Message[
		TeXDelimited::string,
		Length[{rest}] + 1,
		HoldForm[TeXDelimited[rest, end, opts]]
	];
	$Failed
)


TeXDelimited /:
	MakeBoxes[
		TeXDelimited[
			start_String,
			body___,
			end_String,
			opts:OptionsPattern[]
		]
		,
		TraditionalForm
	] :=
		With[
			{
				delimSeparator =
					OptionValue[TeXDelimited, opts, "DelimSeparator"]
			}
			,
			ToBoxes[
				TeXVerbatim @ StringJoin[
					start
					,
					StringReplace[
						StringJoin[
							If[Length[{body}] > 0,
								delimSeparator
							(* else *),
								""
							]
							,
							Riffle[
								OptionValue[
									TeXDelimited, opts, "BodyConverter"
								] /@
									{body}
								,
								OptionValue[
									TeXDelimited, opts, "BodySeparator"
								]
							]
						]
						,
						"\n" ->
							"\n" <>
							OptionValue[TeXDelimited, opts, "Indentation"]
					]
					,
					delimSeparator
					,
					end
				]
				,
				TraditionalForm
			]
		]


(* ::Subsection:: *)
(*TeXCommandArgument*)


TeXCommandArgument[optArgs_List, argConverter_] :=
	StringJoin[
		"["
		,
		Riffle[
			Replace[
				optArgs
				,
				{
					(Rule | RuleDelayed)[argName_, value_] :>
						argConverter[argName] <>
						"=" <>
						argConverter[value]
					,
					arg_ :> argConverter[arg]
				}
				,
				{1}
			]
			, 
			","
		]
		,
		"]"
	]
	
TeXCommandArgument[arg_, argConverter_] := "{" <> argConverter[arg] <> "}"


(* ::Subsection:: *)
(*TeXCommand*)


Options[TeXCommand] = {"ArgumentConverter" -> (ToString[#, TeXForm]&)}


TeXCommand[] := (
	Message[TeXCommand::argm, HoldForm[TeXCommand], 0, 1];
	$Failed
)

TeXCommand[name:Except[_String], rest___] := (
	Message[TeXCommand::string, 1, HoldForm[TeXCommand[name, rest]]];
	$Failed
)

TeXCommand[name_, args:Except[_List], rest___] := (
	Message[TeXCommand::list, 2, HoldForm[TeXCommand[name, args, rest]]];
	$Failed
)


TeXCommand /:
	MakeBoxes[
		TeXCommand[name_String, args_List:{}, opts:OptionsPattern[]],
		TraditionalForm
	] :=
		ToBoxes[
			TeXVerbatim @ StringJoin[
				"\\"
				,
				name
				,
				TeXCommandArgument[
					#,
					OptionValue[TeXCommand, opts, "ArgumentConverter"]
				]& /@
					args
			]
			,
			TraditionalForm
		]


(* ::Subsection:: *)
(*TeXEnvironment*)


TeXEnvironment::StrOrListWithStr =
"String or List with first element being String expected at position 1 in `1`."


Options[TeXEnvironment] = {
	"ArgumentConverter" -> (ToString[#, TeXForm]&),
	"BodyConverter" -> (ToString[#, TeXForm]&),
	"Separator" -> "\n",
	"Indentation" -> "    "
}


TeXEnvironment[] := (
	Message[TeXEnvironment::argm, HoldForm[TeXEnvironment], 0, 1];
	$Failed
)

TeXEnvironment[name:Except[_String | {_String, ___}], rest___] := (
	Message[
		TeXEnvironment::StrOrListWithStr,
		HoldForm[TeXEnvironment[name, rest]]
	];
	$Failed
)

TeXEnvironment[name:_String, rest___] := TeXEnvironment[{name}, rest]


TeXEnvironment /:
	MakeBoxes[
		TeXEnvironment[
			{name_String, args_List:{}, opts:OptionsPattern[]},
			body___
		]
		,
		TraditionalForm
	] :=
		ToBoxes[
			TeXDelimited[
				StringJoin[
					"\\begin{", name, "}"
					,
					TeXCommandArgument[
						#,
						OptionValue[TeXEnvironment, opts, "ArgumentConverter"]
					]& /@
						args
				]
				,
				body
				,
				"\\end{" <> name <> "}"
				,
				FilterRules[
					Join[Flatten[{opts}], Options[TeXEnvironment]],
					Options[TeXDelimited]
				]
			]
			,
			TraditionalForm
		]


End[]


(* ::Section:: *)
(*Public symbols protection*)


(*
	Protect all symbols in this context
	(all public symbols provided by this package)
*)
Protect["`*"]


EndPackage[]
