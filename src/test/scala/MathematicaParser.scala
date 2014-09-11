package org.refptr.parsing.mathematica
package tests

import org.specs2.mutable.Specification
import org.specs2.matcher.ParserMatchers
import scala.util.parsing.combinator.RegexParsers

class ExtraParsersSuite extends Specification with ParserMatchers {

    object TestParser extends RegexParsers with ExtraParsers {
        def rule0: Parser[Any] = rule2 | rule3
        def rule1: Parser[Any] = "!!" | notFollowedBy("!", '=', '~')
        def rule2: Parser[Any] = rule1 ~ "=" ~ "\\d+".r
        def rule3: Parser[Any] = rule1 ~ "~" ~ "[a-z]+".r
    }

    val parsers = TestParser

    "Extra parsers" should {
        "Implement notFollowedBy()" in {
            import TestParser.{rule0,rule1}

            rule1 must succeedOn("!")
            rule1 must failOn("!=")
            rule1 must failOn("!~")
            rule1 must succeedOn("!!")
            rule1 must succeedOn("!!=").partially
            rule1 must succeedOn("!!~").partially
            rule0 must succeedOn("! =127")
            rule0 must succeedOn("! ~abc")
            rule0 must succeedOn("! = 127")
            rule0 must succeedOn("! ~ abc")
            rule0 must failOn("!=127")
            rule0 must failOn("!~abc")
            rule0 must failOn("!= 127")
            rule0 must failOn("!~ abc")
        }
    }
}

class MathematicaParserSuite extends Specification {
    import MathematicaParser.parse
    import Implicits._

    protected implicit class MatchString(input: String) {
        def ~==(output: Expr) = parse(input) === ParseResult(output)
    }

    "Mathematica's language ASTs" should {
        "Support implicit conversions" in {
            Eval('f, 17) === Eval('f, Num("17"))
            Eval('f, 1.7) === Eval('f, Num("1.7"))
            Eval('f, "str") === Eval('f, Str("str"))
            Eval('f, 'xyz) === Eval('f, Sym("xyz"))
            Eval('f, true) === Eval('f, 'True)
            Eval('f, false) === Eval('f, 'False)
        }

        "Have head attribute matching AST class name" in {
            'Plus(1, 2, 3).head === Sym("Plus")
            'Times(1, 2, 3).head === Sym("Times")
            'Power(1, 2).head === Sym("Power")
        }

        "Allow to pretty print themselves" in {
            'Plus(1, 2, 3).toPrettyForm === "Plus[1, 2, 3]"
            'Plus('x, 'y, 'z).toPrettyForm === "Plus[x, y, z]"
            'Plus(1, 'Power(2, 3)).toPrettyForm === "Plus[1, Power[2, 3]]"
            'Span(1, 'All).toPrettyForm === "Span[1, All]"
            'f('x, 'y, 'z).toPrettyForm === "f[x, y, z]"
        }

        "Produce readable output from toString" in {
            'Plus(1, 2, 3).toString === "Eval(Sym(Plus), Num(1), Num(2), Num(3))"
            'Plus('x, 'y, 'z).toString === "Eval(Sym(Plus), Sym(x), Sym(y), Sym(z))"
            'Plus(1, 'Power(2, 3)).toString === "Eval(Sym(Plus), Num(1), Eval(Sym(Power), Num(2), Num(3)))"
            'Span(1, 'All).toString === "Eval(Sym(Span), Num(1), Sym(All))"
            'f('x, 'y, 'z).toString === "Eval(Sym(f), Sym(x), Sym(y), Sym(z))"
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
            "-x" ~== 'Times(-1, Sym("x"))
            "-xyz" ~== 'Times(-1, Sym("xyz"))
            "-\"abc\"" ~== 'Times(-1, Str("abc"))
        }

        "Parse symbols and patterns" in {
            "xyz"               ~== Sym("xyz")
            "$MachinePrecision" ~== Sym("$MachinePrecision")

            "_"                 ~== 'Blank()
            "_Integer"          ~== 'Blank('Integer)
            "x_"                ~== 'Pattern('x, 'Blank())
            "x_Integer"         ~== 'Pattern('x, 'Blank('Integer))
            "xyz_"              ~== 'Pattern('xyz, 'Blank())
            "xyz_Integer"       ~== 'Pattern('xyz, 'Blank('Integer))

            "__"                ~== 'BlankSequence()
            "__Integer"         ~== 'BlankSequence('Integer)
            "x__"               ~== 'Pattern('x, 'BlankSequence())
            "x__Integer"        ~== 'Pattern('x, 'BlankSequence('Integer))
            "xyz__"             ~== 'Pattern('xyz, 'BlankSequence())
            "xyz__Integer"      ~== 'Pattern('xyz, 'BlankSequence('Integer))

            "___"               ~== 'BlankNullSequence()
            "___Integer"        ~== 'BlankNullSequence('Integer)
            "x___"              ~== 'Pattern('x, 'BlankNullSequence())
            "x___Integer"       ~== 'Pattern('x, 'BlankNullSequence('Integer))
            "xyz___"            ~== 'Pattern('xyz, 'BlankNullSequence())
            "xyz___Integer"     ~== 'Pattern('xyz, 'BlankNullSequence('Integer))
        }

        "Parse output references: %, %%, %%%, %n" in {
            "%"          ~== 'Out()
            "%%"         ~== 'Out(-2)
            "%%%"        ~== 'Out(-3)
            "%0"         ~== 'Out(0)
            "%1"         ~== 'Out(1)
            "%127"       ~== 'Out(127)
            "% + 123"    ~== 'Plus('Out(), 123)
            "2*%%%"      ~== 'Times(2, 'Out(-3))
            "%17!"       ~== 'Factorial('Out(17))
        }

        "Parse slots: #, #127, ##, ##127" in {
            "#"           ~== 'Slot(1)
            "#127"        ~== 'Slot(127)
            "a #"         ~== 'Times('a, 'Slot(1))
            "a #127"      ~== 'Times('a, 'Slot(127))
            "a # + 1"     ~== 'Plus('Times('a, 'Slot(1)), 1)
            "a #127 + 1"  ~== 'Plus('Times('a, 'Slot(127)), 1)

            "##"          ~== 'SlotSequence(1)
            "##127"       ~== 'SlotSequence(127)
            "a ##"        ~== 'Times('a, 'SlotSequence(1))
            "a ##127"     ~== 'Times('a, 'SlotSequence(127))
            "a ## + 1"    ~== 'Plus('Times('a, 'SlotSequence(1)), 1)
            "a ##127 + 1" ~== 'Plus('Times('a, 'SlotSequence(127)), 1)

            "###"         ~== 'Times('SlotSequence(1), 'Slot(1))
            "## #"        ~== 'Times('SlotSequence(1), 'Slot(1))
            "# ##"        ~== 'Times('Slot(1), 'SlotSequence(1))
        }

        "Parse mixed arithmetics" in {
            "1 + 2"         ~== 'Plus(1, 2)
            "1 + 2 + 3"     ~== 'Plus(1, 2, 3)
            "1 + 2 + 3 + 4" ~== 'Plus(1, 2, 3, 4)
            "1 * 2"         ~== 'Times(1, 2)
            "1 * 2 * 3"     ~== 'Times(1, 2, 3)
            "1 * 2 * 3 * 4" ~== 'Times(1, 2, 3, 4)
            "1 + 2 * 3"     ~== 'Plus(1, 'Times(2, 3))
            "1 + (2 * 3)"   ~== 'Plus(1, 'Times(2, 3))
            "(1 + 2) * 3"   ~== 'Times('Plus(1, 2), 3)
            "1 * 2 + 3"     ~== 'Plus('Times(1, 2), 3)
            "(1 * 2) + 3"   ~== 'Plus('Times(1, 2), 3)
            "1 * (2 + 3)"   ~== 'Times(1, 'Plus(2, 3))
        }

        "Parse explicit and implied multiplication" in {
            "127 * x"       ~== 'Times(127, 'x)
            "127*x"         ~== 'Times(127, 'x)
            "127 x"         ~== 'Times(127, 'x)
            "127x"          ~== 'Times(127, 'x)
            "x*y"           ~== 'Times('x, 'y)
            "x y"           ~== 'Times('x, 'y)
            "xy"            ~== 'xy
            "xy z"          ~== 'Times('xy, 'z)
            "x*y*z"         ~== 'Times('x, 'y, 'z)
            "x y z"         ~== 'Times('x, 'y, 'z)
            "-x y z"        ~== 'Times('Times(-1, 'x), 'y, 'z)
            "-x -y z"       ~== 'Plus('Times(-1, 'x), 'Times(-1, 'Times('y, 'z)))
            "-x -y -z"      ~== 'Plus('Times(-1, 'x), 'Times(-1, 'y), 'Times(-1, 'z))
            "-127*Exp[x]"   ~== 'Times(-127, 'Exp('x))
            "-127 Exp[x]"   ~== 'Times(-127, 'Exp('x))
            "-127Exp[x]"    ~== 'Times(-127, 'Exp('x))
            "Exp[x]*Exp[y]" ~== 'Times('Exp('x), 'Exp('y))
            "Exp[x] Exp[y]" ~== 'Times('Exp('x), 'Exp('y))
            "Exp[x]Exp[y]"  ~== 'Times('Exp('x), 'Exp('y))
        }

        "Parse exponentials" in {
            "1^2"           ~== 'Power(1, 2)
            "1^2^3"         ~== 'Power(1, 'Power(2, 3))
            "1^2^3^4"       ~== 'Power(1, 'Power(2, 'Power(3, 4)))
            "(1^2)^3^4"     ~== 'Power('Power(1, 2), 'Power(3, 4))
            "1^(2^3)^4"     ~== 'Power(1, 'Power('Power(2, 3), 4))
            "1^2^(3^4)"     ~== 'Power(1, 'Power(2, 'Power(3, 4)))
            "(1^2^3)^4"     ~== 'Power('Power(1, 'Power(2, 3)), 4)
            "1^(2^3^4)"     ~== 'Power(1, 'Power(2, 'Power(3, 4)))
            "x^y"           ~== 'Power('x, 'y)
            "x^y^z"         ~== 'Power('x, 'Power('y, 'z))
            "x^y^z^t"       ~== 'Power('x, 'Power('y, 'Power('z, 't)))
            "(x^y)^z^t"     ~== 'Power('Power('x, 'y), 'Power('z, 't))
            "x^(y^z)^t"     ~== 'Power('x, 'Power('Power('y, 'z), 't))
            "x^y^(z^t)"     ~== 'Power('x, 'Power('y, 'Power('z, 't)))
            "(x^y^z)^t"     ~== 'Power('Power('x, 'Power('y, 'z)), 't)
            "x^(y^z^t)"     ~== 'Power('x, 'Power('y, 'Power('z, 't)))
        }

        "Parse exponentials with negative values" in {
            "-1^-2"         ~== 'Times(-1, 'Power(1, -2))
            "-1^-2^-3"      ~== 'Times(-1, 'Power(1, 'Times(-1, 'Power(2, -3))))
            "-1^-2^-3^-4"   ~== 'Times(-1, 'Power(1, 'Times(-1, 'Power(2, 'Times(-1, 'Power(3, -4))))))
            "(-1^-2)^-3^-4" ~== 'Power('Times(-1, 'Power(1, -2)), 'Times(-1, 'Power(3, -4)))
            "-1^(-2^-3)^-4" ~== 'Times(-1, 'Power(1, 'Power('Times(-1, 'Power(2, -3)), -4)))
            "-1^-2^(-3^-4)" ~== 'Times(-1, 'Power(1, 'Times(-1, 'Power(2, 'Times(-1, 'Power(3, -4))))))
            "(-1^-2^-3)^-4" ~== 'Power('Times(-1, 'Power(1, 'Times(-1, 'Power(2, -3)))), -4)
            "-1^(-2^-3^-4)" ~== 'Times(-1, 'Power(1, 'Times(-1, 'Power(2, 'Times(-1, 'Power(3, -4))))))
            "-x^-y"         ~== 'Times(-1, 'Power('x, 'Times(-1, 'y)))
            "-x^-y^-z"      ~== 'Times(-1, 'Power('x, 'Times(-1, 'Power('y, 'Times(-1, 'z)))))
            "-x^-y^-z^-t"   ~== 'Times(-1, 'Power('x, 'Times(-1, 'Power('y, 'Times(-1, 'Power('z, 'Times(-1, 't)))))))
            "(-x^-y)^-z^-t" ~== 'Power('Times(-1, 'Power('x, 'Times(-1, 'y))), 'Times(-1, 'Power('z, 'Times(-1, 't))))
            "-x^(-y^-z)^-t" ~== 'Times(-1, 'Power('x, 'Power('Times(-1, 'Power('y, 'Times(-1, 'z))), 'Times(-1, 't))))
            "-x^-y^(-z^-t)" ~== 'Times(-1, 'Power('x, 'Times(-1, 'Power('y, 'Times(-1, 'Power('z, 'Times(-1, 't)))))))
            "(-x^-y^-z)^-t" ~== 'Power('Times(-1, 'Power('x, 'Times(-1, 'Power('y, 'Times(-1, 'z))))), 'Times(-1, 't))
            "-x^(-y^-z^-t)" ~== 'Times(-1, 'Power('x, 'Times(-1, 'Power('y, 'Times(-1, 'Power('z, 'Times(-1, 't)))))))
        }

        "Parse rules: x -> y" in {
            "x -> y"                 ~== 'Rule('x, 'y)
            "x -> y -> z"            ~== 'Rule('x, 'Rule('y, 'z))
            "(x -> y) -> z"          ~== 'Rule('Rule('x, 'y), 'z)
            "x -> (y -> z)"          ~== 'Rule('x, 'Rule('y, 'z))
            "x -> y -> z -> t"       ~== 'Rule('x, 'Rule('y, 'Rule('z, 't)))
            "(x -> y) -> z -> t"     ~== 'Rule('Rule('x, 'y), 'Rule('z, 't))
            "x -> (y -> z) -> t"     ~== 'Rule('x, 'Rule('Rule('y, 'z), 't))
            "x -> y -> (z -> t)"     ~== 'Rule('x, 'Rule('y, 'Rule('z, 't)))
            "(x -> y -> z) -> t"     ~== 'Rule('Rule('x, 'Rule('y, 'z)), 't)
            "x -> (y -> z -> t)"     ~== 'Rule('x, 'Rule('y, 'Rule('z, 't)))
            "-x -> y + z -> 3*t"     ~== 'Rule('Times(-1, 'x), 'Rule('Plus('y, 'z), 'Times(3, 't)))
            "x -> y || z -> t"       ~== 'Rule('x, 'Rule('Or('y, 'z), 't))
            "(x -> y) || (z -> t)"   ~== 'Or('Rule('x, 'y), 'Rule('z, 't))

            "x :> y"                 ~== 'DelayedRule('x, 'y)
            "x :> y :> z"            ~== 'DelayedRule('x, 'DelayedRule('y, 'z))
            "(x :> y) :> z"          ~== 'DelayedRule('DelayedRule('x, 'y), 'z)
            "x :> (y :> z)"          ~== 'DelayedRule('x, 'DelayedRule('y, 'z))
            "x :> y :> z :> t"       ~== 'DelayedRule('x, 'DelayedRule('y, 'DelayedRule('z, 't)))
            "x :> y || z :> t"       ~== 'DelayedRule('x, 'DelayedRule('Or('y, 'z), 't))
            "(x :> y) || (z :> t)"   ~== 'Or('DelayedRule('x, 'y), 'DelayedRule('z, 't))

            "a -> b -> c :> d :> e -> f :> g" ~== 'Rule('a, 'Rule('b, 'DelayedRule('c, 'DelayedRule('d, 'Rule('e, 'DelayedRule('f, 'g))))))
        }

        "Parse replace all: x /. y" in {
            "x /. y"             ~== 'ReplaceAll('x, 'y)
            "x /. y /. z"        ~== 'ReplaceAll('ReplaceAll('x, 'y), 'z)
            "(x /. y) /. z"      ~== 'ReplaceAll('ReplaceAll('x, 'y), 'z)
            "x /. (y /. z)"      ~== 'ReplaceAll('x, 'ReplaceAll('y, 'z))
            "x -> y /. z -> t"   ~== 'ReplaceAll('Rule('x, 'y), 'Rule('z, 't))
            "x -> (y /. z) -> t" ~== 'Rule('x, 'Rule('ReplaceAll('y, 'z), 't))
        }

        "Parse assignment operators: =, :=, +=, -=, *=, /=" in {
            "x  = y /. z"           ~== 'Set('x, 'ReplaceAll('y, 'z))
            "x  = y /. z  = t -> f" ~== 'Set('x, 'Set('ReplaceAll('y, 'z), 'Rule('t, 'f)))
            "x := y /. z"           ~== 'SetDelayed('x, 'ReplaceAll('y, 'z))
            "x := y /. z := t -> f" ~== 'SetDelayed('x, 'SetDelayed('ReplaceAll('y, 'z), 'Rule('t, 'f)))
            "x += y /. z"           ~== 'AddTo('x, 'ReplaceAll('y, 'z))
            "x += y /. z += t -> f" ~== 'AddTo('x, 'AddTo('ReplaceAll('y, 'z), 'Rule('t, 'f)))
            "x -= y /. z"           ~== 'SubtractFrom('x, 'ReplaceAll('y, 'z))
            "x -= y /. z -= t -> f" ~== 'SubtractFrom('x, 'SubtractFrom('ReplaceAll('y, 'z), 'Rule('t, 'f)))
            "x *= y /. z"           ~== 'TimesBy('x, 'ReplaceAll('y, 'z))
            "x *= y /. z *= t -> f" ~== 'TimesBy('x, 'TimesBy('ReplaceAll('y, 'z), 'Rule('t, 'f)))
            "x /= y /. z"           ~== 'DivideBy('x, 'ReplaceAll('y, 'z))
            "x /= y /. z /= t -> f" ~== 'DivideBy('x, 'DivideBy('ReplaceAll('y, 'z), 'Rule('t, 'f)))

            "a + 1 = b + 2 := c + 3 += d + 4 -= e + 5 *= f + 6 /= g + 7" ~==
                'Set('Plus('a, 1), 'SetDelayed('Plus('b, 2), 'AddTo('Plus('c, 3), 'SubtractFrom('Plus('d, 4),
                     'TimesBy('Plus('e, 5), 'DivideBy('Plus('f, 6), 'Plus('g, 7)))))))
        }

        "Parse compound expressions: a; b; c" in {
            "x = 1"                ~== 'Set('x, 1)
            "x = 1;"               ~== 'CompoundExpression('Set('x, 1), 'Null)
            "x = 1; y = 2"         ~== 'CompoundExpression('Set('x, 1), 'Set('y, 2))
            "x = 1; y = 2;"        ~== 'CompoundExpression('Set('x, 1), 'Set('y, 2), 'Null)
            "x = 1; y = 2; z = 3"  ~== 'CompoundExpression('Set('x, 1), 'Set('y, 2), 'Set('z, 3))
            "x = 1; y = 2; z = 3;" ~== 'CompoundExpression('Set('x, 1), 'Set('y, 2), 'Set('z, 3), 'Null)

            "(x; y); z; t"         ~== 'CompoundExpression('CompoundExpression('x, 'y), 'z, 't)
            "x; (y; z); t"         ~== 'CompoundExpression('x, 'CompoundExpression('y, 'z), 't)
            "x; y; (z; t)"         ~== 'CompoundExpression('x, 'y, 'CompoundExpression('z, 't))

            "(x; y) z; t"          ~== 'CompoundExpression('Times('CompoundExpression('x, 'y), 'z), 't)
            "(x; y;) z; t"         ~== 'CompoundExpression('Times('CompoundExpression('x, 'y, 'Null), 'z), 't)
        }

        "Parse factorials: x!" in {
            "x!"         ~== 'Factorial('x)
            "x! + b"     ~== 'Plus('Factorial('x), 'b)
            "a + x!"     ~== 'Plus('a, 'Factorial('x))
            "a + x! + b" ~== 'Plus('a, 'Factorial('x), 'b)
            "-x!"        ~== 'Times(-1, 'Factorial('x))
            "127 x!"     ~== 'Times(127, 'Factorial('x))
            "a x!"       ~== 'Times('a, 'Factorial('x))
            "x! b"       ~== 'Times('Factorial('x), 'b)
            "x!*b"       ~== 'Times('Factorial('x), 'b)
            "a x! b"     ~== 'Times('a, 'Factorial('x), 'b)
            "x^2!"       ~== 'Power('x, 'Factorial(2))
            "(a x)!"     ~== 'Factorial('Times('a, 'x))
            "x!^a b"     ~== 'Times('Power('Factorial('x), 'a), 'b)
            // "x! !"       ~== 'Factorial('Factorial('x))
            // "x! !y"      ~== 'Times('Factorial('Factorial('x)), 'y)
            // "x!*!y"      ~== 'Times('Factorial('x), 'Not('y))
            "(x!)!"      ~== 'Factorial('Factorial('x))
            "(x!)!^a"    ~== 'Power('Factorial('Factorial('x)), 'a)
        }

        "Parse double factorials: x!!" in {
            "x!!"        ~== 'Factorial2('x)
            "x!!!"       ~== 'Factorial('Factorial2('x))
            "x! !!"      ~== 'Factorial2('Factorial('x))
            "(x!)!!"     ~== 'Factorial2('Factorial('x))
            "(x!!)!"     ~== 'Factorial('Factorial2('x))
            "x! ! !"     ~== 'Factorial('Factorial('Factorial('x)))
            "x!! !!"     ~== 'Factorial2('Factorial2('x))
            "x!! !!^a"   ~== 'Power('Factorial2('Factorial2('x)), 'a)
            "x!! !!!^a"  ~== 'Power('Factorial('Factorial2('Factorial2('x))), 'a)
        }

        "Parse equivalence operators: a === b, a =!= b" in {
            "x === y"                     ~== 'SameQ('x, 'y)
            "!x === y"                    ~== 'Not('SameQ('x, 'y))
            "x === !y"                    ~== 'SameQ('x, 'Not('y))
            "x === y === z"               ~== 'SameQ('x, 'y, 'z)
            "!x === y === z"              ~== 'Not('SameQ('x, 'y, 'z))
            "x === !y === z"              ~== 'SameQ('x, 'Not('SameQ('y, 'z)))
            "!x === y === z === t"        ~== 'Not('SameQ('x, 'y, 'z, 't))
            "x + 1 === a y === z! === -t" ~== 'SameQ('Plus('x, 1), 'Times('a, 'y), 'Factorial('z), 'Times(-1, 't))
            "x === y == z === t"          ~== 'SameQ('x, 'Equal('y, 'z), 't)
            "x == y === z == t"           ~== 'SameQ('Equal('x, 'y), 'Equal('z, 't))
            "x === y || z === t"          ~== 'Or('SameQ('x, 'y), 'SameQ('z, 't))
            "x === y && z === t"          ~== 'And('SameQ('x, 'y), 'SameQ('z, 't))
            "x === y || !z === t"         ~== 'Or('SameQ('x, 'y), 'Not('SameQ('z, 't)))
            "x === y && !z === t"         ~== 'And('SameQ('x, 'y), 'Not('SameQ('z, 't)))
            "!x === y || !z === t"        ~== 'Or('Not('SameQ('x, 'y)), 'Not('SameQ('z, 't)))
            "!x === y && !z === t"        ~== 'And('Not('SameQ('x, 'y)), 'Not('SameQ('z, 't)))
            "x -> y === z -> t"           ~== 'Rule('x, 'Rule('SameQ('y, 'z), 't))

            "x =!= y"                     ~== 'UnsameQ('x, 'y)
            "!x =!= y"                    ~== 'Not('UnsameQ('x, 'y))
            "x =!= !y"                    ~== 'UnsameQ('x, 'Not('y))
            "x =!= y =!= z"               ~== 'UnsameQ('x, 'y, 'z)
            "!x =!= y =!= z"              ~== 'Not('UnsameQ('x, 'y, 'z))
            "x =!= !y =!= z"              ~== 'UnsameQ('x, 'Not('UnsameQ('y, 'z)))
            "!x =!= y =!= z =!= t"        ~== 'Not('UnsameQ('x, 'y, 'z, 't))

            "a === b === c =!= d =!= e =!= f === g" ~== 'SameQ('UnsameQ('SameQ('a, 'b, 'c), 'd, 'e, 'f), 'g)
        }

        "Parse logical operators: x || y, x && y" in {
            "x || y"               ~== 'Or('x, 'y)
            "x || y || z"          ~== 'Or('x, 'y, 'z)
            "x || y || z || t"     ~== 'Or('x, 'y, 'z, 't)
            "x || (y || z) || t"   ~== 'Or('x, 'Or('y, 'z), 't)
            "x && y"               ~== 'And('x, 'y)
            "x && y && z"          ~== 'And('x, 'y, 'z)
            "x && y && z && t"     ~== 'And('x, 'y, 'z, 't)
            "x && (y && z) && t"   ~== 'And('x, 'And('y, 'z), 't)
            "x || y && z || t"     ~== 'Or('x, 'And('y, 'z), 't)
            "!x || !y && !z || !t" ~== 'Or('Not('x), 'And('Not('y), 'Not('z)), 'Not('t))
        }

        "Parse comparison operators: x == y, x != y, x <= y, x < y, x >= y, x > y" in {
            "x == y"               ~== 'Equal('x, 'y)
            "x == y == z"          ~== 'Equal('x, 'y, 'z)
            "x == y == z == t"     ~== 'Equal('x, 'y, 'z, 't)
            "x != y"               ~== 'Unequal('x, 'y)
            "x != y != z"          ~== 'Unequal('x, 'y, 'z)
            "x != y != z != t"     ~== 'Unequal('x, 'y, 'z, 't)
            "x <= y"               ~== 'LessEqual('x, 'y)
            "x <= y <= z"          ~== 'LessEqual('x, 'y, 'z)
            "x <= y <= z <= t"     ~== 'LessEqual('x, 'y, 'z, 't)
            "x <  y"               ~== 'Less('x, 'y)
            "x <  y <  z"          ~== 'Less('x, 'y, 'z)
            "x <  y <  z <  t"     ~== 'Less('x, 'y, 'z, 't)
            "x >= y"               ~== 'GreaterEqual('x, 'y)
            "x >= y >= z"          ~== 'GreaterEqual('x, 'y, 'z)
            "x >= y >= z >= t"     ~== 'GreaterEqual('x, 'y, 'z, 't)
            "x >  y"               ~== 'Greater('x, 'y)
            "x >  y >  z"          ~== 'Greater('x, 'y, 'z)
            "x >  y >  z >  t"     ~== 'Greater('x, 'y, 'z, 't)

            "x! == y"              ~== 'Equal('Factorial('x), 'y)
            "x! != y"              ~== 'Unequal('Factorial('x), 'y)
            "x!!= y"               ~== 'Set('Factorial2('x), 'y)
            "x!! = y"              ~== 'Set('Factorial2('x), 'y)
            "x!!!= y"              ~== 'Unequal('Factorial2('x), 'y)

            "!x == y"              ~== 'Not('Equal('x, 'y))
            "!x == y == z"         ~== 'Not('Equal('x, 'y, 'z))
            "!x != y"              ~== 'Not('Unequal('x, 'y))
            "!x != y != z"         ~== 'Not('Unequal('x, 'y, 'z))
            "!x <= y"              ~== 'Not('LessEqual('x, 'y))
            "!x <= y <= z"         ~== 'Not('LessEqual('x, 'y, 'z))
            "!x <  y"              ~== 'Not('Less('x, 'y))
            "!x <  y <  z"         ~== 'Not('Less('x, 'y, 'z))
            "!x >= y"              ~== 'Not('GreaterEqual('x, 'y))
            "!x >= y >= z"         ~== 'Not('GreaterEqual('x, 'y, 'z))
            "!x >  y"              ~== 'Not('Greater('x, 'y))
            "!x >  y >  z"         ~== 'Not('Greater('x, 'y, 'z))

            "a == b != c <= d < e >= f > g" ~== 'Inequality('a, 'Equal, 'b, 'Unequal,
                'c, 'LessEqual, 'd, 'Less, 'e, 'GreaterEqual, 'f, 'Greater, 'g)
            "-a == 127 b + 1 != c^3 <= d!" ~== 'Inequality('Times(-1, 'a), 'Equal,
                'Plus('Times(127, 'b), 1), 'Unequal, 'Power('c, 3), 'LessEqual, 'Factorial('d))
        }

        "Parse indexing operator: x[[0, 1, 2, 3]]" in {
            "x[[0]]"                    ~== 'Part('x, 0)
            "x[[0, 1, 2, 3]]"           ~== 'Part('x, 0, 1, 2, 3)
            "x[[0]][[1]]"               ~== 'Part('Part('x, 0), 1)
            "x[[0]][[1]][[2]]"          ~== 'Part('Part('Part('x, 0), 1), 2)
            "x[[0]][[1]][[2]][[3]]"     ~== 'Part('Part('Part('Part('x, 0), 1), 2), 3)
            "x[[0]][[1,2]][[3,4,5]]"    ~== 'Part('Part('Part('x, 0), 1, 2), 3, 4, 5)

            "-x[[0]]"                   ~== 'Times(-1, 'Part('x, 0))
            "3x[[0]]"                   ~== 'Times( 3, 'Part('x, 0))
            "a x[[0]]"                  ~== 'Times('a, 'Part('x, 0))
            "x^a[[0]]"                  ~== 'Power('x, 'Part('a, 0))
            "x[[0]]^a"                  ~== 'Power('Part('x, 0), 'a)
            "x[[0]]^y[[1]]"             ~== 'Power('Part('x, 0), 'Part('y, 1))
            "x[[0]] = 127"              ~== 'Set('Part('x, 0), 127)
            "{x[[0]], y[[0]]} = {a, b}" ~== 'Set('List('Part('x, 0), 'Part('y, 0)), 'List('a, 'b))

            "x[[y[[f[t]]]]]"            ~== 'Part('x, 'Part('y, 'f('t)))
            "-x[[-y[[f[t]]]]]"          ~== 'Times(-1, 'Part('x, 'Times(-1, 'Part('y, 'f('t)))))
        }

        "Parse span operators: a ;; b, a ;; b ;; c" in {
            "a ;; b"                    ~== 'Span('a, 'b)
            "a ;;  "                    ~== 'Span('a, 'All)
            "  ;; b"                    ~== 'Span(1, 'b)
            "  ;;  "                    ~== 'Span(1, 'All)
            "a ;; b ;; c"               ~== 'Span('a, 'b, 'c)
            "a ;;   ;; c"               ~== 'Span('a, 'All, 'c)
            "  ;; b ;; c"               ~== 'Span(1, 'b, 'c)
            "  ;;   ;; c"               ~== 'Span(1, 'All, 'c)

            "a + b ;; c + d"            ~== 'Span('Plus('a, 'b), 'Plus('c, 'd))
            "a b ;; c d"                ~== 'Span('Times('a, 'b), 'Times('c, 'd))
            "a^b ;; c^d"                ~== 'Span('Power('a, 'b), 'Power('c, 'd))
            "-a ;; -b"                  ~== 'Span('Times(-1, 'a), 'Times(-1, 'b))
            "a! ;; b!"                  ~== 'Span('Factorial('a), 'Factorial('b))

            "a ;; b ; c ;; d"           ~== 'CompoundExpression('Span('a, 'b), 'Span('c, 'd))
            "a ;; b /. c ;; d"          ~== 'ReplaceAll('Span('a, 'b), 'Span('c, 'd))
            "a ;; b -> c ;; d"          ~== 'Rule('Span('a, 'b), 'Span('c, 'd))
            "a ;; b = c ;; d"           ~== 'Set('Span('a, 'b), 'Span('c, 'd))
            "a ;; b := c ;; d"          ~== 'SetDelayed('Span('a, 'b), 'Span('c, 'd))
            "a ;; b === c ;; d"         ~== 'SameQ('Span('a, 'b), 'Span('c, 'd))
            "a ;; b == c ;; d"          ~== 'Equal('Span('a, 'b), 'Span('c, 'd))
            "a ;; b != c ;; d"          ~== 'Unequal('Span('a, 'b), 'Span('c, 'd))
            "a ;; b || c ;; d"          ~== 'Or('Span('a, 'b), 'Span('c, 'd))
            "a ;; b && c ;; d"          ~== 'And('Span('a, 'b), 'Span('c, 'd))
            "! a ;; b ;; c"             ~== 'Not('Span('a, 'b, 'c))

            "x[[0;;1;;2, a;;b;;c]]"     ~== 'Part('x, 'Span(0, 1, 2), 'Span('a, 'b, 'c))
        }

        "Parse pattern test operator: a ? b" in {
            "x_Integer ? Prime"         ~== 'PatternTest('Pattern('x, 'Blank('Integer)), 'Prime)
            "-_ ? Even"                 ~== 'Times(-1, 'PatternTest('Blank(), 'Even))
            "a b ? Odd"                 ~== 'Times('a, 'PatternTest('b, 'Odd))
            "_^_? Real"                 ~== 'Power('Blank(), 'PatternTest('Blank(), 'Real))
            "_ ? Integer!"              ~== 'Factorial('PatternTest('Blank(), 'Integer))
            // "_! ? Positive"             ~== 'PatternTest('Factorial('Blank()), 'Positive)
            // "_! ? Positive!"            ~== 'Factorial('PatternTest('Factorial('Blank()), 'Positive))
            "_? Positive[[0]]"          ~== 'Part('PatternTest('Blank(), 'Positive), 0)
            // "_[[0]] ? Positive"         ~== 'PatternTest('Part('Blank(), 0), 'Positive)
            // "_[[0]] ? Positive[[1]]"    ~== 'Part('PatternTest('Part('Blank(), 0), 'Positive), 1)
        }

        "Parse condition operator: a /; b" in {
            "x /; y"                ~== 'Condition('x, 'y)
            "x /; y /; z"           ~== 'Condition('Condition('x, 'y), 'z)
            "-x /; y /; z"          ~== 'Condition('Condition('Times(-1, 'x), 'y), 'z)
            "x + 127 /; y! /; z"    ~== 'Condition('Condition('Plus('x, 127), 'Factorial('y)), 'z)
            "127 x /; y! /; z == t" ~== 'Condition('Condition('Times(127, 'x), 'Factorial('y)), 'Equal('z, 't))
            "a x /; y! /; z ? t"    ~== 'Condition('Condition('Times('a , 'x), 'Factorial('y)), 'PatternTest('z, 't))
            "x^127 /; y! /; z || t" ~== 'Condition('Condition('Power('x, 127), 'Factorial('y)), 'Or('z, 't))
            "!x /; y! /; z == t"    ~== 'Condition('Condition('Not('x), 'Factorial('y)), 'Equal('z, 't))

            "x -> t /; y /; z"      ~== 'Rule('x, 'Condition('Condition('t, 'y), 'z))
            "x /; y -> t /; z"      ~== 'Rule('Condition('x, 'y), 'Condition('t, 'z))
            "x /; y /; z -> t"      ~== 'Rule('Condition('Condition('x, 'y), 'z), 't)
        }

        "Parse functional evaluation: f[x], (f + g)[x][y][z]" in {
            "f[x]"                  ~== 'f('x)
            "f[x, y]"               ~== 'f('x, 'y)
            "f[x, y, z]"            ~== 'f('x, 'y, 'z)
            "f[x][y]"               ~== 'f('x)('y)
            "f[x][y][z]"            ~== 'f('x)('y)('z)
            "f[x][x, y]"            ~== 'f('x)('x, 'y)
            "f[x][x, y][x, y, z]"   ~== 'f('x)('x, 'y)('x, 'y, 'z)
            "f[g[x]]"               ~== 'f('g('x))
            "f[g[h[x, y, z][t]]]"   ~== 'f('g('h('x, 'y, 'z)('t)))
            "f[x][y, z][[0]]"       ~== 'Part('f('x)('y, 'z), 0)

            "f + g[x]"              ~== 'Plus('f, 'g('x))
            "f g[x]"                ~== 'Times('f, 'g('x))
            "f^g[x]"                ~== 'Power('f, 'g('x))
            "f![x]"                 ~== 'Factorial('f)('x)
            "f[[0]][x]"             ~== 'Part('f, 0)('x)
            "f ? g[x]"              ~== 'PatternTest('f, 'g)('x)
            "(f + g)[x]"            ~== 'Plus('f, 'g)('x)
            "(f + g)[x]^h[a, b]"    ~== 'Power('Plus('f, 'g)('x), 'h('a, 'b))
        }

        "Parse derivatives (Lagrange's notation): f'''[t]" in {
            "f'"                    ~== 'Derivative(1)('f)
            "f''"                   ~== 'Derivative(2)('f)
            "f'''"                  ~== 'Derivative(3)('f)
            "f''''"                 ~== 'Derivative(4)('f)
            "f''' ' ''''"           ~== 'Derivative(4)('Derivative(1)('Derivative(3)('f)))
            "f'[t]"                 ~== 'Derivative(1)('f)('t)
            "f'[t]''"               ~== 'Derivative(2)('Derivative(1)('f)('t))
            "f'[t]''[x,y]"          ~== 'Derivative(2)('Derivative(1)('f)('t))('x, 'y)
            "f'[t]''[x,y]'''"       ~== 'Derivative(3)('Derivative(2)('Derivative(1)('f)('t))('x, 'y))
            "f + g'"                ~== 'Plus('f, 'Derivative(1)('g))
            "f g'"                  ~== 'Times('f, 'Derivative(1)('g))
            "f^g'"                  ~== 'Power('f, 'Derivative(1)('g))
            "f[t]'"                 ~== 'Derivative(1)('f('t))
            "f!'"                   ~== 'Derivative(1)('Factorial('f))
            "f[[0]]'"               ~== 'Derivative(1)('Part('f, 0))
            "f?g'"                  ~== 'Derivative(1)('PatternTest('f, 'g))
            "(f + g)'"              ~== 'Derivative(1)('Plus('f, 'g))
        }

        "Parse increment and decrement operators: x++, y--" in {
            "x++"                   ~== 'Increment('x)
            "x++++"                 ~== 'Increment('Increment('x))
            "x++++++"               ~== 'Increment('Increment('Increment('x)))
            "x--"                   ~== 'Decrement('x)
            "x----"                 ~== 'Decrement('Decrement('x))
            "x------"               ~== 'Decrement('Decrement('Decrement('x)))
            "x++--"                 ~== 'Decrement('Increment('x))
            "x--++"                 ~== 'Increment('Decrement('x))

            "x!++"                  ~== 'Increment('Factorial('x))
            "x++!"                  ~== 'Factorial('Increment('x))
            "x[0]++"                ~== 'Increment('x(0))
            "x++[0]"                ~== 'Increment('x)(0)
            "x[[1]]++"              ~== 'Increment('Part('x, 1))
            "x++[[1]]"              ~== 'Part('Increment('x), 1)
            "x'++"                  ~== 'Increment('Derivative(1)('x))
            "x++'"                  ~== 'Derivative(1)('Increment('x))
        }

        "Parse dot product: a . b" in {
            "a . b"                 ~== 'Dot('a, 'b)
            "a . b . c"             ~== 'Dot('a, 'b, 'c)
            "a . b . c . d"         ~== 'Dot('a, 'b, 'c, 'd)
            "a * b . c . d * e"     ~== 'Times('a, 'Dot('b, 'c, 'd), 'e)
            "a . b ^ c"             ~== 'Dot('a, 'Power('b, 'c))
        }

        "Parse functions: # + 1 &" in {
            "#&"                    ~== 'Function('Slot(1))
            "a + b &"               ~== 'Function('Plus('a, 'b))
            "a b &"                 ~== 'Function('Times('a, 'b))
            // "a + b & + 1"           ~== 'Plus('Function('Plus('a, 'b)), 1)
        }

        "Parse repeated patterns: a.., b..." in {
            "a.."                   ~== 'Repeated('a)
            "a..."                  ~== 'RepeatedNull('a)
            "a....."                ~== 'Repeated('RepeatedNull('a))
            "a.. ..."               ~== 'RepeatedNull('Repeated('a))
            "a... .."               ~== 'Repeated('RepeatedNull('a))

            "a || b.."              ~== 'Repeated('Or('a, 'b))
            "a /; b.."              ~== 'Condition('a, 'Repeated('b))
            "a || b..."             ~== 'RepeatedNull('Or('a, 'b))
            "a /; b..."             ~== 'Condition('a, 'RepeatedNull('b))
        }

        "Parse map operator: Function[x, x^2] /@ {1, 2, 3, 4}" in {
            "f /@ g"                ~== 'Map('f, 'g)
            "f /@ g /@ h"           ~== 'Map('f, 'Map('g, 'h))

            "Function[x, x^2] /@ {1, 2, 3, 4}" ~== 'Map('Function('x, 'Power('x, 2)), 'List(1, 2, 3, 4))
        }

        "Parse application operator: f @@ {1, 2, 3, 4}" in {
            "f @@ g"                ~== 'Apply('f, 'g)
            "f @@ g @@ h"           ~== 'Apply('f, 'Apply('g, 'h))

            "f @@ g /@ h @@ t"      ~== 'Apply('f, 'Map('g, 'Apply('h, 't)))
            "f @@ {1, 2, 3, 4}"     ~== 'Apply('f, 'List(1, 2, 3, 4))
        }

        "Parse postfix evaluation operator: x // f" in {
            "x // f"                ~== 'f('x)
            "x // f // g"           ~== 'g('f('x))

            "x = y // f"            ~== 'Set('x, 'f('y))
            "x /. y // f"           ~== 'f('ReplaceAll('x, 'y))
            "x& // y&"              ~== 'Function('y)('Function('x))
        }

        "Parse alternatives operator: a | b" in {
            "a | b"                 ~== 'Alternatives('a, 'b)
            "a | b | c"             ~== 'Alternatives('a, 'b, 'c)
            "a | b | c | d"         ~== 'Alternatives('a, 'b, 'c, 'd)

            "{a, b, c} /. a|b -> x" ~== 'ReplaceAll('List('a, 'b, 'c), 'Rule('Alternatives('a, 'b), 'x))
        }

        "Parse real life examples" in {
            "DSolve[{y'[x] + y[x] == a Sin[x], y[0] == 0}, y, x]; FullSimplify[y''[x] + y[x]^2 /. %]" ~==
                'CompoundExpression(
                    'DSolve(
                        'List(
                            'Equal(
                                'Plus('Derivative(1)('y)('x), Eval('y, 'x)),
                                'Times('a, Eval('Sin, 'x))),
                            'Equal(Eval('y, 0), 0)),
                        'y, 'x),
                    'FullSimplify(
                        'ReplaceAll(
                            'Plus(
                                'Derivative(2)('y)('x),
                                'Power(Eval('y, 'x), 2)),
                            'Out())))
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
