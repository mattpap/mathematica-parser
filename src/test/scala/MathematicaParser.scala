package org.sympy.parsing.mathematica.tests

import org.specs2.mutable.Specification

import org.sympy.parsing.mathematica.{MathematicaParser,Expr,Sym,Num,Str,Eval}
import org.sympy.parsing.mathematica.{Plus,Times,Power}
import org.sympy.parsing.mathematica.MathematicaImplicits._

class MathematicaParserSuite extends Specification {
    protected implicit class MatchString(input: String) {
        def ~==(output: Expr) = MathematicaParser.parse(input) === Some(output)
    }

    "Mathematica's language parser" should {
        "Parse constants" in {
            "1" ~== Num("1")
            "2" ~== Num("2")
        }

        "Parse arithmetics" in {
            "1 + 2"         ~== Plus(1, 2)
            "1 + 2 + 3"     ~== Plus(Plus(1, 2), 3)
            "1 + 2 + 3 + 4" ~== Plus(Plus(Plus(1, 2), 3), 4)
            "1 * 2"         ~== Times(1, 2)
            "1 * 2 * 3"     ~== Times(Times(1, 2), 3)
            "1 * 2 * 3 * 4" ~== Times(Times(Times(1, 2), 3), 4)
            "1 + 2 * 3"     ~== Plus(1, Times(2, 3))
            "1 + (2 * 3)"   ~== Plus(1, Times(2, 3))
            "(1 + 2) * 3"   ~== Times(Plus(1, 2), 3)
            "1 * 2 + 3"     ~== Plus(Times(1, 2), 3)
            "(1 * 2) + 3"   ~== Plus(Times(1, 2), 3)
            "1 * (2 + 3)"   ~== Times(1, Plus(2, 3))
            "1^2"           ~== Power(1, 2)
            // "1^2^3"         ~== Power(1, Power(2, 3))
            // "1^2^3^4"       ~== Power(1, Power(2, Power(3, 4)))
        }
    }
}
