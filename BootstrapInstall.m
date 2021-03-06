(* ::Package:: *)

Get["https://raw.githubusercontent.com/jkuczm/MathematicaBootstrapInstaller/v0.1.1/BootstrapInstaller.m"]


BootstrapInstall[
	"TeXUtilities",
	"https://github.com/jkuczm/MathematicaTeXUtilities/releases/download/v1.0.1/TeXUtilities.zip",
	"AdditionalFailureMessage" ->
		Sequence[
			"You can ",
			Hyperlink[
				"install TeXUtilities package manually",
				"https://github.com/jkuczm/MathematicaTeXUtilities#manual-installation"
			],
			"."
		]
]
