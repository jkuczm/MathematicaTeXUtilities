(* ::Package:: *)

If[Quiet[Needs["ProjectInstaller`"], {Get::noopen, Needs::nocont}] === $Failed,
	Print @ Import[
		"https://raw.github.com/lshifr/ProjectInstaller/master/BootstrapInstall.m"
	];
	Needs["ProjectInstaller`"];
];

Print @ ProjectInstall @ URL[
	"https://github.com/jkuczm/MathematicaTeXUtilities/releases/download/v1.0.0/TeXUtilities.zip"
];
