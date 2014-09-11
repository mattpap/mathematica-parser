Mathematica Parser
==================

A library for parsing Mathematica's programming language written in Scala.
It uses parser combinators and packrat parsers from Scala's standard library.
Currently only a subset of Mathematica's language is supported.

Supported features
------------------

* function applications: `f[x]`, `PrimeQ[a]`
* arithmetics: `a + b`, `a b`, `a*b`, `a/b`, `a^b`
* patterns: `_`, `x_`, `x_Integer`
* assignment operators: `f[n_] := 1`, `x += 127`
* conditions: `f[n_ /; n > 0] := 1`, `n ? PrimeQ`
* rules: `Factor[x^2 + 1, Modulus->2]`
* compound expressions: `x = 0; x += 1`
* factorials: `x!`, `x!!`
* indexing and slices: `a[[0;;3]] = 1`
* logical and comparison operators: `x == 0 || a < b <= c`

Operator grouping is preserved so `a + b + c` parses as `Plus[a, b, c]`, not
`Plus[Plus[a, b], c]`, and `a^b^c` parses as `Power[a, Power[b, c]]`. No
flattening is performed, nor other transformations applied by default that could
change structure of abstract syntax trees.

See `src/test/scala/MathematicaParser.scala` for examples.

Usage
-----

Run `./sbt`. This can take awhile on the first run, because it has to download
runner's and project's dependencies (e.g. Scala compiler). Then you can use the
following commands:

* `compile`        : compile the project
* `test`           : run tests
* `console`        : run Scala REPL (imports are done automatically)
* `run -e "2 + 2"` : parse and output full form of the input expression (see `run --help`)

Note that `compile` is automatically invoked by `test` and others if needed.

Example
-------

```
~/mathematica-parser$ ./sbt
[info] Loading project definition from ~/mathematica-parser/project
[info] Set current project to mathematica_parser (in build file:~/mathematica-parser/)
refptr (mathematica_parser)> console
[info] Compiling 1 Scala source to ~/mathematica-parser/target/scala-2.10/classes...
[info] Starting scala interpreter...
[info]
import org.refptr.parsing.mathematica._
Welcome to Scala version 2.10.0-M4 (OpenJDK 64-Bit Server VM, Java 1.6.0_23).
Type in expressions to have them evaluated.
Type :help for more information.

scala> MathematicaParser.parse("1 + 2*3")
res0: org.refptr.parsing.mathematica.ParseOutput = ParseResult(Plus(Num(1), Times(Num(2), Num(3))))

scala> res0.toPrettyForm
res1: String = Plus[1, Times[2, 3]]
```

License
-------

Copyright &copy; 2012-2014 by Mateusz Paprocki and contributors.

Published under The MIT License, see LICENSE.
