# TeX Utilities

[![latest release](http://img.shields.io/github/release/jkuczm/MathematicaTeXUtilities.svg)](https://github.com/jkuczm/MathematicaTeXUtilities/releases/latest)
[![Semantic Versioning](http://jkuczm.github.io/media/images/SemVer-2.0.0-brightgreen.svg)](http://semver.org/spec/v2.0.0.html)
[![license MIT](http://jkuczm.github.io/media/images/license-MIT-blue.svg)](https://github.com/jkuczm/MathematicaTeXUtilities/blob/master/LICENSE)


Set of tools useful for customizing TeX output of Mathematica.

Contains also patch fixing behavior of `TeXForm` for expressions with custom
formatting defined using `Format[expr, TeXForm]`.


* [Usage example](#usage-example)
* [Installation](#installation)
    * [Automatic installation](#automatic-installation)
    * [Manual installation](#manual-installation)
    * [No installation](#no-installation)
* [Documentation](#documentation)
* [Compatibility](#compatibility)
* [Bugs and requests](#bugs-and-requests)
* [Contributing](#contributing)
* [License](#license)
* [Versioning](#versioning)



## Usage example

Define custom TeX formatting for some symbols:

```Mathematica
Format[something, TeXForm] = TeXVerbatim["\\macro $1+1$ \\command[a=b]{c}"];

Format[f[x__], TeXForm] := TeXDelimited["\\left(", x, "\\right)", "DelimSeparator" -> ""]

Format[g[x__], TeXForm] := TeXCommand["g", {{a -> b}, x}]

Format[h[x__], TeXForm] := TeXEnvironment["myEnv", x]
```

Use `TeXForm` as usual:

```Mathematica
TeXForm[
    h[
        f[1 + alpha],
        5 - g[3, g[2]],
        something
    ]
]
```

Output is:
```TeX
\begin{myEnv}
    \left(\alpha +1\right)
    5-\g[a=b]{3}{\g[a=b]{2}}
    \macro $1+1$ \command[a=b]{c}
\end{myEnv}
```

You can find more usage examples in
[package documentation](http://jkuczm.github.io/MathematicaTeXUtilities/reference/guide/TeXUtilities.html).



## Installation


### Automatic installation

To install TeXUtilities package evaluate:
```Mathematica
Get["https://raw.githubusercontent.com/jkuczm/MathematicaTeXUtilities/master/BootstrapInstall.m"]
```

Note that this will also install
[ProjectInstaller](https://github.com/lshifr/ProjectInstaller) package, if you
don't have it already installed.

To load TeXUtilities package evaluate: ``Needs["TeXUtilities`"]``.


### Manual installation

1. Download latest released
   [TeXUtilities.zip](https://github.com/jkuczm/MathematicaTeXUtilities/releases/download/v1.0.0/TeXUtilities.zip)
   file.

2. Extract downloaded `TeXUtilities.zip` to any directory which is on Mathematica `$Path`,
   e.g. to one obtained by evaluating `FileNameJoin[{$UserBaseDirectory,"Applications"}]`.


3. To load the package evaluate: ``Needs["TeXUtilities`"]``.


### No installation

To use package directly from the Web, without installation, evaluate:
```Mathematica
Get["https://raw.githubusercontent.com/jkuczm/MathematicaTeXUtilities/master/NoInstall.m"]
```

Note that, with this method of initialization,
package documentation will not be available in Mathematica Documentation Center,
but you can use
[online version of documentation](http://jkuczm.github.io/MathematicaTeXUtilities/reference/guide/TeXUtilities.html).


## Documentation

This application comes with documentation integrated with Mathematica Documentation Center.
To use it search for "TeXUtilities" in documentation center
or press `F1` key with cursor on name of any of symbols introduced by this application.

There's also
[online version of documentation](http://jkuczm.github.io/MathematicaTeXUtilities/reference/guide/TeXUtilities.html).



## Compatibility

This package contains extensive automatic test suite. Package is being tested
with Mathematica versions 8.0, 9.0 and 10.0 on Linux. Since it doesn't contain
any OS specific code it should work with above versions on all operating
systems.

There's also no obvious reason for package not to work on earlier (6.0+)
versions of Mathematica.



## Bugs and requests

If you find any bugs or have feature request please create an
[issue on GitHub](https://github.com/jkuczm/MathematicaTeXUtilities/issues).



## Contributing

Feel free to fork and send pull requests.

If you want to use Ant scripts from this repository you will also need to
install [WWBCommon](https://github.com/jkuczm/WWBCommon) project.

All contributions are welcome!



## License

This package is released under
[The MIT License](https://github.com/jkuczm/MathematicaTeXUtilities/blob/master/LICENSE).



## Versioning

Releases of this package will be numbered using
[Semantic Versioning guidelines](http://semver.org/).
