FormatTeXFormPatch
==================

Patch fixing behavior of `TeXForm` for expressions with custom formatting
defined using `Format[expr, TeXForm]`.



Installation and Initialization
------------------------------

There are two ways to initialize this package:

1. Download `FormatTeXFormPatch/FormatTeXFormPatch.m` file and put it somewhere
on your Mathematica `$Path`.
To load the package use ``Needs["FormatTeXFormPatch`"]``.

2. Get package contents directly from GitHub:
```Mathematica
Get["https://raw.githubusercontent.com/jkuczm/MathematicaFormatTeXFormPatch/master/FormatTeXFormPatch/FormatTeXFormPatch.m"]
```



Usage example
-------------

Define custom TeX formatting for some symbols:

```Mathematica
Format[h[x__], TeXForm] :=
    TeXVerbatim["\\h" <> ("{" <> ToString[#, TeXForm] <> "}" & /@ {x})];

Format[myEnv[x__], TeXForm] := 
    TeXVerbatim[
        "\\begin{myEnv}" <>
        ("\n    " <> ToString[#, TeXForm] <> "\\\\" & /@ {x}) <>
        "\n\\end{myEnv}"
    ];
```

Use `TeXForm` as usual:

```Mathematica
TeXForm[myEnv[1 + alpha, 5 - h[3, h[2]]]]
```
Output is:
```TeX
\begin{myEnv}
    \alpha +1\\
    5-\h{3}{\h{2}}\\
\end{myEnv}
```
