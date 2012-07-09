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
            Plus('x, 'y, 'z).toPrettyForm === "Plus[x, y, z]"
            Plus(1, Power(2, 3)).toPrettyForm === "Plus[1, Power[2, 3]]"
            Span(1, All).toPrettyForm === "Span[1, All]"
        }

        "Have produce readable output from toString" in {
            Plus(1, 2, 3).toString === "Plus(Num(1), Num(2), Num(3))"
            Plus('x, 'y, 'z).toString === "Plus(Sym(x), Sym(y), Sym(z))"
            Plus(1, Power(2, 3)).toString === "Plus(Num(1), Power(Num(2), Num(3)))"
            Span(1, All).toString === "Span(Num(1), All)"
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

        "Parse rules: x -> y" in {
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

        "Parse replace all: x /. y" in {
            "x /. y"             ~== ReplaceAll('x, 'y)
            "x /. y /. z"        ~== ReplaceAll(ReplaceAll('x, 'y), 'z)
            "(x /. y) /. z"      ~== ReplaceAll(ReplaceAll('x, 'y), 'z)
            "x /. (y /. z)"      ~== ReplaceAll('x, ReplaceAll('y, 'z))
            "x -> y /. z -> t"   ~== ReplaceAll(Rule('x, 'y), Rule('z, 't))
            "x -> (y /. z) -> t" ~== Rule('x, Rule(ReplaceAll('y, 'z), 't))
        }

        "Parse assignment operators: =, :=, +=, -=, *=, /=" in {
            "x  = y /. z"           ~== Set('x, ReplaceAll('y, 'z))
            "x  = y /. z  = t -> f" ~== Set('x, Set(ReplaceAll('y, 'z), Rule('t, 'f)))
            "x := y /. z"           ~== SetDelayed('x, ReplaceAll('y, 'z))
            "x := y /. z := t -> f" ~== SetDelayed('x, SetDelayed(ReplaceAll('y, 'z), Rule('t, 'f)))
            "x += y /. z"           ~== AddTo('x, ReplaceAll('y, 'z))
            "x += y /. z += t -> f" ~== AddTo('x, AddTo(ReplaceAll('y, 'z), Rule('t, 'f)))
            "x -= y /. z"           ~== SubtractFrom('x, ReplaceAll('y, 'z))
            "x -= y /. z -= t -> f" ~== SubtractFrom('x, SubtractFrom(ReplaceAll('y, 'z), Rule('t, 'f)))
            "x *= y /. z"           ~== TimesBy('x, ReplaceAll('y, 'z))
            "x *= y /. z *= t -> f" ~== TimesBy('x, TimesBy(ReplaceAll('y, 'z), Rule('t, 'f)))
            "x /= y /. z"           ~== DivideBy('x, ReplaceAll('y, 'z))
            "x /= y /. z /= t -> f" ~== DivideBy('x, DivideBy(ReplaceAll('y, 'z), Rule('t, 'f)))

            "a + 1 = b + 2 := c + 3 += d + 4 -= e + 5 *= f + 6 /= g + 7" ~==
                Set(Plus('a, 1), SetDelayed(Plus('b, 2), AddTo(Plus('c, 3), SubtractFrom(Plus('d, 4),
                    TimesBy(Plus('e, 5), DivideBy(Plus('f, 6), Plus('g, 7)))))))
        }

        "Parse compound expressions: a; b; c" in {
            "x = 1"                ~== Set('x, 1)
            "x = 1;"               ~== CompoundExpression(Set('x, 1), Null)
            "x = 1; y = 2"         ~== CompoundExpression(Set('x, 1), Set('y, 2))
            "x = 1; y = 2;"        ~== CompoundExpression(Set('x, 1), Set('y, 2), Null)
            "x = 1; y = 2; z = 3"  ~== CompoundExpression(Set('x, 1), Set('y, 2), Set('z, 3))
            "x = 1; y = 2; z = 3;" ~== CompoundExpression(Set('x, 1), Set('y, 2), Set('z, 3), Null)

            "(x; y); z; t"         ~== CompoundExpression(CompoundExpression('x, 'y), 'z, 't)
            "x; (y; z); t"         ~== CompoundExpression('x, CompoundExpression('y, 'z), 't)
            "x; y; (z; t)"         ~== CompoundExpression('x, 'y, CompoundExpression('z, 't))

            "(x; y) z; t"          ~== CompoundExpression(Times(CompoundExpression('x, 'y), 'z), 't)
            "(x; y;) z; t"         ~== CompoundExpression(Times(CompoundExpression('x, 'y, Null), 'z), 't)
        }

        "Parse factorials: x!" in {
            "x!"         ~== Factorial('x)
            "x! + b"     ~== Plus(Factorial('x), 'b)
            "a + x!"     ~== Plus('a, Factorial('x))
            "a + x! + b" ~== Plus(Plus('a, Factorial('x)), 'b)
            "-x!"        ~== Times(-1, Factorial('x))
            "127 x!"     ~== Times(127, Factorial('x))
            "a x!"       ~== Times('a, Factorial('x))
            "x! b"       ~== Times(Factorial('x), 'b)
            "x!*b"       ~== Times(Factorial('x), 'b)
            "a x! b"     ~== Times(Times('a, Factorial('x)), 'b)
            "x^2!"       ~== Power('x, Factorial(2))
            "(a x)!"     ~== Factorial(Times('a, 'x))
            "x!^a b"     ~== Times(Power(Factorial('x), 'a), 'b)
            "x! !"       ~== Factorial(Factorial('x))
            "x! !y"      ~== Times(Factorial(Factorial('x)), 'y)
            "x!*!y"      ~== Times(Factorial('x), Not('y))
            "(x!)!"      ~== Factorial(Factorial('x))
            "(x!)!^a"    ~== Power(Factorial(Factorial('x)), 'a)
        }

        "Parse double factorials: x!!" in {
            "x!!"        ~== Factorial2('x)
            "x!!!"       ~== Factorial(Factorial2('x))
            "x! !!"      ~== Factorial2(Factorial('x))
            "(x!)!!"     ~== Factorial2(Factorial('x))
            "(x!!)!"     ~== Factorial(Factorial2('x))
            "x! ! !"     ~== Factorial(Factorial(Factorial('x)))
            "x!! !!"     ~== Factorial2(Factorial2('x))
            "x!! !!^a"   ~== Power(Factorial2(Factorial2('x)), 'a)
            "x!! !!!^a"  ~== Power(Factorial(Factorial2(Factorial2('x))), 'a)
        }

        "Parse equivalence operator: a === b" in {
            "x === y"                     ~== SameQ('x, 'y)
            "!x === y"                    ~== Not(SameQ('x, 'y))
            "x === !y"                    ~== SameQ('x, Not('y))
            "x === y === z"               ~== SameQ('x, 'y, 'z)
            "!x === y === z"              ~== Not(SameQ('x, 'y, 'z))
            "x === !y === z"              ~== SameQ('x, Not(SameQ('y, 'z)))
            "!x === y === z === t"        ~== Not(SameQ('x, 'y, 'z, 't))
            "x + 1 === a y === z! === -t" ~== SameQ(Plus('x, 1), Times('a, 'y), Factorial('z), Times(-1, 't))
            "x === y == z === t"          ~== SameQ('x, Equal('y, 'z), 't)
            "x == y === z == t"           ~== SameQ(Equal('x, 'y), Equal('z, 't))
            "x === y || z === t"          ~== Or(SameQ('x, 'y), SameQ('z, 't))
            "x === y && z === t"          ~== And(SameQ('x, 'y), SameQ('z, 't))
            "x === y || !z === t"         ~== Or(SameQ('x, 'y), Not(SameQ('z, 't)))
            "x === y && !z === t"         ~== And(SameQ('x, 'y), Not(SameQ('z, 't)))
            "!x === y || !z === t"        ~== Or(Not(SameQ('x, 'y)), Not(SameQ('z, 't)))
            "!x === y && !z === t"        ~== And(Not(SameQ('x, 'y)), Not(SameQ('z, 't)))
            "x -> y === z -> t"           ~== Rule('x, Rule(SameQ('y, 'z), 't))
        }

        "Parse output references: %, %%, %%%, %n" in {
            "%"          ~== Out()
            "%%"         ~== Out(-2)
            "%%%"        ~== Out(-3)
            "%0"         ~== Out(0)
            "%1"         ~== Out(1)
            "%127"       ~== Out(127)
            "% + 123"    ~== Plus(Out(), 123)
            "2*%%%"      ~== Times(2, Out(-3))
            "%17!"       ~== Factorial(Out(17))
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
