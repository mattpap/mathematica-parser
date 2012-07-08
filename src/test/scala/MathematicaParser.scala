package org.sympy.parsing.mathematica.tests

import org.specs2.mutable.Specification

import org.sympy.parsing.mathematica.{MathematicaParser,ParseResult,ParseError}
import org.sympy.parsing.mathematica.{Expr,Sym,Num,Str,Eval}
import org.sympy.parsing.mathematica.{Plus,Times,Power,Span,All,True,False}
import org.sympy.parsing.mathematica.MathematicaImplicits._

class MathematicaParserSuite extends Specification {
    import MathematicaParser.parse

    protected implicit class MatchString(input: String) {
        def ~==(output: Expr) = parse(input) === ParseResult(output)
    }

    "Mathematica's language ASTs" should {
        "Support implicit conversions" in {
            Eval("Some", 17) === Eval("Some", Num("17"))
            Eval("Some", 1.7) === Eval("Some", Num("1.7"))
            Eval("Some", "str") === Eval("Some", Str("str"))
            Eval("Some", 'xyz) === Eval("Some", Sym("xyz"))
            Eval("Some", true) === Eval("Some", True)
            Eval("Some", false) === Eval("Some", False)
        }

        "Have head attribute matching AST class name" in {
            Plus(1, 2, 3).head === "Plus"
            Times(1, 2, 3).head === "Times"
            Power(1, 2).head === "Power"
            All.head === "All"
        }

        "Allow to pretty print themselves" in {
            Plus(1, 2, 3).toPrettyForm === "Plus[1, 2, 3]"
            Times(1, 2, 3).toPrettyForm === "Times[1, 2, 3]"
            Plus(1, Power(2, 3)).toPrettyForm === "Plus[1, Power[2, 3]]"
            Span(1, All).toPrettyForm === "Span[1, All]"
        }
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

        "Deal with errors" in {
            parse("1 + ") must beLike {
                case ParseError(msg, file, pos) =>
                    msg must contain("but end of source found")
                    file === "<string>"
                    pos.line === 1
                    pos.column === 5
            }
        }
    }
}
