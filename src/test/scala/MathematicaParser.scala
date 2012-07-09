package org.sympy.parsing.mathematica.tests

import org.specs2.mutable.Specification

import org.sympy.parsing.mathematica.{MathematicaParser,ParseResult,ParseError}
import org.sympy.parsing.mathematica.{Expr,Sym,Num,Str,Eval,Builtins,Singletons,Implicits}

class MathematicaParserSuite extends Specification {
    import MathematicaParser.parse

    import Builtins._
    import Singletons._
    import Implicits._

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
        "Parse values" in {
            "1" ~== Num("1")
            "127" ~== Num("127")
            "2.56" ~== Num("2.56")
            "x" ~== Sym("x")
            "xyz" ~== Sym("xyz")
            "\"abc\"" ~== Str("abc")
        }

        "Parse negative values" in {
            "-1" ~== Num("-1")
            "-127" ~== Num("-127")
            "-2.56" ~== Num("-2.56")
            "-x" ~== Times(-1, Sym("x"))
            "-xyz" ~== Times(-1, Sym("xyz"))
            "-\"abc\"" ~== Times(-1, Str("abc"))
        }

        "Parse mixed arithmetics" in {
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
        }

        "Parse explicit and implied multiplication" in {
            "127 * x"       ~== Times(127, 'x)
            "127*x"         ~== Times(127, 'x)
            "127 x"         ~== Times(127, 'x)
            "127x"          ~== Times(127, 'x)
            "x*y"           ~== Times('x, 'y)
            "x y"           ~== Times('x, 'y)
            "xy"            ~== 'xy
            "xy z"          ~== Times('xy, 'z)
            "x*y*z"         ~== Times(Times('x, 'y), 'z)
            "x y z"         ~== Times(Times('x, 'y), 'z)
            "-x y z"        ~== Times(Times(Times(-1, 'x), 'y), 'z)
            "-x -y z"       ~== Times(Times(Times(-1, 'x), Times(-1, 'y)), 'z)
            "-x -y -z"      ~== Times(Times(Times(-1, 'x), Times(-1, 'y)), Times(-1, 'z))
            "-127*Exp[x]"   ~== Times(-127, Exp('x))
            "-127 Exp[x]"   ~== Times(-127, Exp('x))
            "-127Exp[x]"    ~== Times(-127, Exp('x))
            "Exp[x]*Exp[y]" ~== Times(Exp('x), Exp('y))
            "Exp[x] Exp[y]" ~== Times(Exp('x), Exp('y))
            "Exp[x]Exp[y]"  ~== Times(Exp('x), Exp('y))
        }

        "Parse exponentials" in {
            "1^2"           ~== Power(1, 2)
            "1^2^3"         ~== Power(1, Power(2, 3))
            "1^2^3^4"       ~== Power(1, Power(2, Power(3, 4)))
            "(1^2)^3^4"     ~== Power(Power(1, 2), Power(3, 4))
            "1^(2^3)^4"     ~== Power(1, Power(Power(2, 3), 4))
            "1^2^(3^4)"     ~== Power(1, Power(2, Power(3, 4)))
            "(1^2^3)^4"     ~== Power(Power(1, Power(2, 3)), 4)
            "1^(2^3^4)"     ~== Power(1, Power(2, Power(3, 4)))
            "x^y"           ~== Power('x, 'y)
            "x^y^z"         ~== Power('x, Power('y, 'z))
            "x^y^z^t"       ~== Power('x, Power('y, Power('z, 't)))
            "(x^y)^z^t"     ~== Power(Power('x, 'y), Power('z, 't))
            "x^(y^z)^t"     ~== Power('x, Power(Power('y, 'z), 't))
            "x^y^(z^t)"     ~== Power('x, Power('y, Power('z, 't)))
            "(x^y^z)^t"     ~== Power(Power('x, Power('y, 'z)), 't)
            "x^(y^z^t)"     ~== Power('x, Power('y, Power('z, 't)))
        }

        "Parse exponentials with negative values" in {
            "-1^-2"         ~== Times(-1, Power(1, -2))
            "-1^-2^-3"      ~== Times(-1, Power(1, Times(-1, Power(2, -3))))
            "-1^-2^-3^-4"   ~== Times(-1, Power(1, Times(-1, Power(2, Times(-1, Power(3, -4))))))
            "(-1^-2)^-3^-4" ~== Power(Times(-1, Power(1, -2)), Times(-1, Power(3, -4)))
            "-1^(-2^-3)^-4" ~== Times(-1, Power(1, Power(Times(-1, Power(2, -3)), -4)))
            "-1^-2^(-3^-4)" ~== Times(-1, Power(1, Times(-1, Power(2, Times(-1, Power(3, -4))))))
            "(-1^-2^-3)^-4" ~== Power(Times(-1, Power(1, Times(-1, Power(2, -3)))), -4)
            "-1^(-2^-3^-4)" ~== Times(-1, Power(1, Times(-1, Power(2, Times(-1, Power(3, -4))))))
            "-x^-y"         ~== Times(-1, Power('x, Times(-1, 'y)))
            "-x^-y^-z"      ~== Times(-1, Power('x, Times(-1, Power('y, Times(-1, 'z)))))
            "-x^-y^-z^-t"   ~== Times(-1, Power('x, Times(-1, Power('y, Times(-1, Power('z, Times(-1, 't)))))))
            "(-x^-y)^-z^-t" ~== Power(Times(-1, Power('x, Times(-1, 'y))), Times(-1, Power('z, Times(-1, 't))))
            "-x^(-y^-z)^-t" ~== Times(-1, Power('x, Power(Times(-1, Power('y, Times(-1, 'z))), Times(-1, 't))))
            "-x^-y^(-z^-t)" ~== Times(-1, Power('x, Times(-1, Power('y, Times(-1, Power('z, Times(-1, 't)))))))
            "(-x^-y^-z)^-t" ~== Power(Times(-1, Power('x, Times(-1, Power('y, Times(-1, 'z))))), Times(-1, 't))
            "-x^(-y^-z^-t)" ~== Times(-1, Power('x, Times(-1, Power('y, Times(-1, Power('z, Times(-1, 't)))))))
        }

        "Parse rules" in {
            "x -> y"                 ~== Rule('x, 'y)
            "x -> y -> z"            ~== Rule('x, Rule('y, 'z))
            "(x -> y) -> z"          ~== Rule(Rule('x, 'y), 'z)
            "x -> (y -> z)"          ~== Rule('x, Rule('y, 'z))
            "x -> y -> z -> t"       ~== Rule('x, Rule('y, Rule('z, 't)))
            "(x -> y) -> z -> t"     ~== Rule(Rule('x, 'y), Rule('z, 't))
            "x -> (y -> z) -> t"     ~== Rule('x, Rule(Rule('y, 'z), 't))
            "x -> y -> (z -> t)"     ~== Rule('x, Rule('y, Rule('z, 't)))
            "(x -> y -> z) -> t"     ~== Rule(Rule('x, Rule('y, 'z)), 't)
            "x -> (y -> z -> t)"     ~== Rule('x, Rule('y, Rule('z, 't)))
            "-x -> y + z -> 3*t"     ~== Rule(Times(-1, 'x), Rule(Plus('y, 'z), Times(3, 't)))
            "x -> y || z -> t"       ~== Rule('x, Rule(Or('y, 'z), 't))
            "(x -> y) || (z -> t)"   ~== Or(Rule('x, 'y), Rule('z, 't))
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
