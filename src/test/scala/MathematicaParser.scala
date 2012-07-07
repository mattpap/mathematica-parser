package org.sympy.tests

import org.specs2.mutable.Specification
import org.specs2.matcher.DataTables

import org.sympy.{MathematicaParser,Expr,Sym,Num,Str,Eval}

class MathematicaParserSuite extends Specification with DataTables {
    protected val verify = (input: String, output: Expr) =>
        MathematicaParser.parse(input) === Some(output)

    "Mathematica's language parser" should {
        "Parse constants" in {
            "input" || "output" |>
            "1"     !! Num("1") |
            "2"     !! Num("2") | verify
        }
    }
}
