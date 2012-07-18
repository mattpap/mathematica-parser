package org.sympy.parsing.mathematica

import java.io.File

import scala.annotation.tailrec
import scala.util.matching.Regex

import scala.util.parsing.combinator.{RegexParsers,PackratParsers}
import scala.util.parsing.input.Position

class MathematicaParser extends RegexParsers with PackratParsers with ExtraParsers with ExtraRegexParsers {
    protected override val whiteSpace = """(\s|(?m)\(\*(\*(?!/)|[^*])*\*\))+""".r

    type ExprParser = PackratParser[Expr]

    import Implicits._

    protected val name = "[a-zA-Z$][a-zA-Z0-9$]*"

    lazy val ident: PackratParser[String] = regex(name.r)

    lazy val pattern: ExprParser =
        log(symbolBlankWithHead | symbolBlank | symbol | blankWithHead | blank)("pattern")

    protected def buildBlank(underscores: String): Expr = underscores.length match {
        case 1 => 'Blank
        case 2 => 'BlankSequence
        case 3 => 'BlankNullSequence
    }

    lazy val blank: ExprParser = regexMatch("(_{1,3})".r) ^^ {
        case Regex.Groups(underscores) => buildBlank(underscores)()
    }
    lazy val blankWithHead: ExprParser = regexMatch(s"(_{1,3})($name)".r) ^^ {
        case Regex.Groups(underscores, head) => buildBlank(underscores)(Sym(head))
    }
    lazy val symbol: ExprParser = regexMatch(s"($name)".r) ^^ {
        case Regex.Groups(name) => Sym(name)
    }
    lazy val symbolBlank: ExprParser = regexMatch(s"($name)(_{1,3})".r) ^^ {
        case Regex.Groups(name, underscores) => 'Pattern(Sym(name), buildBlank(underscores)())
    }
    lazy val symbolBlankWithHead: ExprParser = regexMatch(s"($name)(_{1,3})($name)".r) ^^ {
        case Regex.Groups(name, underscores, head) => 'Pattern(Sym(name), buildBlank(underscores)(Sym(head)))
    }

    lazy val number: PackratParser[Num] = log("""(\d+\.\d*|\d*\.\d+|\d+)([eE][+-]?\d+)?""".r)("number") ^^ {
        case value => Num(value)
    }

    lazy val str: PackratParser[Str] = log(("\"" + """(\\.|[^"])*""" + "\"").r)("str") ^^ {
        case value => Str(value.stripPrefix("\"").stripSuffix("\"").replace("\\\\\"", "\""))
    }

    lazy val out: ExprParser = outNumber | outClassic
    lazy val outNumber: ExprParser = "%\\d+".r ^^ {
        case value => 'Out(Num(value.stripPrefix("%")))
    }
    lazy val outClassic: ExprParser = "%+".r ^^ {
        case "%"   => 'Out()
        case value => 'Out(Num(s"-${value.length}"))
    }

    lazy val slot: ExprParser = regexMatch("(#{1,2})(\\d*)".r) ^^ {
        case Regex.Groups("#",  "")    => 'Slot(1)
        case Regex.Groups("#",  index) => 'Slot(index.toInt)
        case Regex.Groups("##", "")    => 'SlotSequence(1)
        case Regex.Groups("##", index) => 'SlotSequence(index.toInt)
    }

    lazy val list: ExprParser = "{" ~ repsep(expr, ",") ~ "}" ^^ {
        case "{" ~ elems ~ "}" => 'List(elems: _*)
    }

    protected def rulesFrom(p: ExprParser): ExprParser =
        precedence.dropWhile(p != _).reduce(_ | _)

    protected def rulesFrom(p: ExprParser, drop: ExprParser): ExprParser =
        precedence.dropWhile(p != _).filterNot(_ == drop).reduce(_ | _)

    // Precedence in increasing order:
    protected val precedence: List[ExprParser] =
        compound  :: // infix, flat  :  ;
        assign    :: // infix, right :  = := += -= *= /=
        func      :: // postfix      :  &
        replace   :: // infix, left  :  /.
        rule      :: // infix, right :  -> :>
        cond      :: // infix, left  :  /;
        repeated  :: // postfix      :  .. ...
        or        :: // infix, flat  :  ||
        and       :: // infix, flat  :  &&
        not       :: // prefix       :  !
        same      :: // infix, flat  :  ===, =!=
        cmp       :: // infix, flat  :  == != <= < >= >
        span      :: // infix, none  :  ;;
        add       :: // infix, flat  :  + -
        mul       :: // infix, flat  :  * /
        dot       :: // infix, flat  :  .
        neg       :: // prefix       :  -
        exp       :: // infix, right :  ^
        postfix   :: // postfix      :  ' [[]] [] ! !! -- ++
        test      :: // infix, none  :  ?
        tightest  ::
        Nil

    lazy val expr: ExprParser = log(precedence.reduce(_ | _))("precedence")

    lazy val compound: ExprParser = log(complexCompound | simpleCompound)("compound")
    lazy val semicolon: PackratParser[String] = notFollowedBy(";", ';')
    lazy val complexCompound: ExprParser = compoundExpr ~ semicolon ~ rep1sep(compoundExpr, semicolon) ~ opt(semicolon) ^^ {
        case elem ~ _ ~ elems ~ None =>
            'CompoundExpression(elem :: elems: _*)
        case elem ~ _ ~ elems ~ _ =>
            'CompoundExpression(elem +: elems :+ Sym("Null"): _*)
    }
    lazy val simpleCompound: ExprParser = compoundExpr ~ semicolon ^^ {
        case elem ~ _ => 'CompoundExpression(elem, Sym("Null"))
    }
    lazy val compoundExpr: ExprParser = rulesFrom(assign)

    lazy val assign: ExprParser = log(assignExpr ~ ("=" | ":=" | "+=" | "-=" | "*=" | "/=") ~ (assign | assignExpr))("assign") ^^ {
        case lhs ~  "=" ~ rhs => 'Set(lhs, rhs)
        case lhs ~ ":=" ~ rhs => 'SetDelayed(lhs, rhs)
        case lhs ~ "+=" ~ rhs => 'AddTo(lhs, rhs)
        case lhs ~ "-=" ~ rhs => 'SubtractFrom(lhs, rhs)
        case lhs ~ "*=" ~ rhs => 'TimesBy(lhs, rhs)
        case lhs ~ "/=" ~ rhs => 'DivideBy(lhs, rhs)
    }
    lazy val assignExpr: ExprParser = rulesFrom(func)

    lazy val func: ExprParser = log((func | funcExpr) ~ notFollowedBy("&", '&'))("func") ^^ {
        case expr ~ _ => 'Function(expr)
    }
    lazy val funcExpr: ExprParser = rulesFrom(replace)

    lazy val replace: ExprParser = log((replace | replaceExpr) ~ "/." ~ replaceExpr)("replace") ^^ {
        case lhs ~ _ ~ rhs => 'ReplaceAll(lhs, rhs)
    }
    lazy val replaceExpr: ExprParser = rulesFrom(rule)

    lazy val rule: ExprParser = log(ruleExpr ~ ("->" | ":>") ~ (rule | ruleExpr))("rule") ^^ {
        case lhs ~ "->" ~ rhs => 'Rule(lhs, rhs)
        case lhs ~ ":>" ~ rhs => 'DelayedRule(lhs, rhs)
    }
    lazy val ruleExpr: ExprParser = rulesFrom(cond)

    lazy val cond: ExprParser = log((cond | condExpr) ~ "/;" ~ condExpr)("cond") ^^ {
        case lhs ~ _ ~ rhs => 'Condition(lhs, rhs)
    }
    lazy val condExpr: ExprParser = rulesFrom(repeated)

    lazy val repeated: ExprParser = log((repeated | repeatedExpr) ~ ("..." | ".."))("repeated") ^^ {
        case expr ~ ".."  => 'Repeated(expr)
        case expr ~ "..." => 'RepeatedNull(expr)
    }
    lazy val repeatedExpr: ExprParser = rulesFrom(or)

    lazy val or: ExprParser = log(orExpr ~ rep1("||" ~> orExpr))("or") ^^ {
        case head ~ tail => 'Or(head :: tail: _*)
    }
    lazy val orExpr: ExprParser = rulesFrom(and)

    lazy val and: ExprParser = log(andExpr ~ rep1("&&" ~> andExpr))("and") ^^ {
        case head ~ tail => 'And(head :: tail: _*)
    }
    lazy val andExpr: ExprParser = rulesFrom(not)

    lazy val not: ExprParser = log(notFollowedBy("!", '=') ~ (not | notExpr))("not") ^^ {
        case _ ~ expr => 'Not(expr)
    }
    lazy val notExpr: ExprParser = rulesFrom(same)

    // XXX: special case of `not' on rhs
    lazy val same: ExprParser = log(sameExpr ~ rep1(("===" | "=!=") ~ (not | sameExpr)))("same") ^^ {
        case head ~ tail =>
            case class OpExpr(op: String, expr: Expr)

            @tailrec def buildAST(init: Expr, rest: List[OpExpr]): Expr = {
                rest match {
                    case Nil =>
                        init
                    case OpExpr(op, _) :: _ =>
                        val (head, tail) = rest.span(_.op == op)
                        val args = init :: head.map(_.expr)
                        val result = (op match {
                            case "===" => 'SameQ
                            case "=!=" => 'UnsameQ
                        })(args: _*)
                        buildAST(result, tail)
                }
            }

            buildAST(head, tail.map { case op ~ expr => OpExpr(op, expr) })
    }
    lazy val sameExpr: ExprParser = rulesFrom(cmp)

    protected def getCmpOp(op: String): Expr = op match {
        case "==" => 'Equal
        case "!=" => 'Unequal
        case "<=" => 'LessEqual
        case "<"  => 'Less
        case ">=" => 'GreaterEqual
        case ">"  => 'Greater
    }

    lazy val cmp: ExprParser = log(cmpExpr ~ rep1(("==" | "!="  | "<=" | "<" | ">=" | ">") ~ cmpExpr))("cmp") ^^ {
        case head ~ tail =>
            tail.collect { case op ~ _ => op }.distinct match {
                case op :: Nil =>
                    val args = head :: tail.collect {
                        case _ ~ expr => expr
                    }
                    getCmpOp(op)(args: _*)
                case _ =>
                    val args = head :: tail.flatMap {
                        case op ~ expr => Seq(getCmpOp(op), expr)
                    }
                    'Inequality(args: _*)
            }
    }
    lazy val cmpExpr: ExprParser = rulesFrom(span)

    lazy val span: ExprParser = log(spanParser)("span")
    lazy val spanParser: ExprParser =
        spanExpr ~ ";;" ~ spanExpr ~ ";;" ~ spanExpr ^^ { case i ~ _ ~ j ~ _ ~ k => 'Span(i, j, k)    } |
        spanExpr ~ ";;"            ~ ";;" ~ spanExpr ^^ { case i ~ _     ~ _ ~ k => 'Span(i, 'All, k) } |
                   ";;" ~ spanExpr ~ ";;" ~ spanExpr ^^ { case     _ ~ j ~ _ ~ k => 'Span(1, j, k)    } |
                   ";;"            ~ ";;" ~ spanExpr ^^ { case     _     ~ _ ~ k => 'Span(1, 'All, k) } |
        spanExpr ~ ";;" ~ spanExpr                   ^^ { case i ~ _ ~ j         => 'Span(i, j)       } |
        spanExpr ~ ";;"                              ^^ { case i ~ _             => 'Span(i, 'All)    } |
                   ";;" ~ spanExpr                   ^^ { case     _ ~ j         => 'Span(1, j)       } |
                   ";;"                              ^^ { case     _             => 'Span(1, 'All)    }
    lazy val spanExpr: ExprParser = rulesFrom(add)

    lazy val add: ExprParser = log(addExpr ~ rep1(("+" | "-") ~ addExpr))("add") ^^ {
        case head ~ tail =>
            val args = head :: tail.map {
                case "+" ~ expr       => expr
                case "-" ~ Num(value) => Num(s"-$value")
                case "-" ~ expr       => 'Times(-1, expr)
            }
            'Plus(args: _*)
    }
    lazy val addExpr: ExprParser = rulesFrom(mul)

    lazy val mul: ExprParser = log(mulImplied | mulExplicit)("mul")
    lazy val mulImplied: ExprParser = mulExpr ~ rep1(mulImpliedRhsExpr) ^^ {
        case head ~ tail =>
            val args = head :: tail
            'Times(args: _*)
    }
    lazy val mulExplicit: ExprParser = mulExpr ~ rep1(("*" | notFollowedBy("/", ';', '.')) ~ mulExpr) ^^ {
        case head ~ tail =>
            val args = head :: tail.map {
                case "*" ~ expr => expr
                case "/" ~ expr => 'Power(expr, -1)
            }
            'Times(args: _*)
    }
    // TODO: what about `not'?
    lazy val mulImpliedRhsExpr: ExprParser = rulesFrom(dot, drop=neg)
    lazy val mulExpr: ExprParser = rulesFrom(dot)

    lazy val dot: ExprParser = log(dotExpr ~ rep1("." ~> dotExpr))("dot") ^^ {
        case head ~ tail => 'Dot(head :: tail: _*)
    }
    lazy val dotExpr: ExprParser = rulesFrom(neg)

    lazy val neg: ExprParser = log("-" ~ (neg | negExpr))("neg") ^^ {
        case _ ~ Num(value) => Num(s"-$value")
        case _ ~ expr => 'Times(-1, expr)
    }
    lazy val negExpr: ExprParser = rulesFrom(exp)

    lazy val exp: ExprParser = log(expLhsExpr ~ "^" ~ (exp | expRhsExpr))("exp") ^^ {
        case lhs ~ _ ~ rhs => 'Power(lhs, rhs)
    }
    lazy val expLhsExpr: ExprParser = rulesFrom(postfix)
    lazy val expRhsExpr: ExprParser = neg | expLhsExpr

    protected sealed trait PostfixOp
    protected case class DerivOp(n: Int) extends PostfixOp
    protected case class PartOp(args: Expr*) extends PostfixOp
    protected case class EvalOp(args: Expr*) extends PostfixOp
    protected case object FactorialOp extends PostfixOp
    protected case object Factorial2Op extends PostfixOp
    protected case object DecrementOp extends PostfixOp
    protected case object IncrementOp extends PostfixOp

    lazy val derivOp: PackratParser[PostfixOp] = "'+".r ^^ {
        case ticks => DerivOp(ticks.length)
    }
    lazy val partOp: PackratParser[PostfixOp] = "[[" ~> repsep(expr, ",") <~ "]]" ^^ {
        case exprs => PartOp(exprs: _*)
    }
    lazy val evalOp: PackratParser[PostfixOp] = "[" ~> repsep(expr, ",") <~ "]" ^^ {
        case exprs => EvalOp(exprs: _*)
    }
    lazy val factorialOp: PackratParser[PostfixOp] = ("!!" | notFollowedBy("!", '=')) ^^ {
        case "!" => FactorialOp
        case "!!" => Factorial2Op
    }
    lazy val postIncDecOp: PackratParser[PostfixOp] = ("--" | "++") ^^ {
        case "--" => DecrementOp
        case "++" => IncrementOp
    }

    lazy val postfix: ExprParser = log(postfixExpr ~ rep1(derivOp | partOp | evalOp | factorialOp | postIncDecOp))("postfix") ^^ {
        case expr ~ ops => ops.foldLeft(expr) {
            case (expr, DerivOp(n)) => 'Derivative(n)(expr)
            case (expr, PartOp(indices @ _*)) => 'Part(expr +: indices: _*)
            case (head, EvalOp(args @ _*)) => head(args: _*)
            case (expr, FactorialOp) => 'Factorial(expr)
            case (expr, Factorial2Op) => 'Factorial2(expr)
            case (expr, DecrementOp) => 'Decrement(expr)
            case (expr, IncrementOp) => 'Increment(expr)
        }
    }
    lazy val postfixExpr: ExprParser = rulesFrom(test)

    lazy val test: ExprParser = log(testExpr ~ "?" ~ testExpr)("test") ^^ {
        case lhs ~ _ ~ rhs => 'PatternTest(lhs, rhs)
    }
    lazy val testExpr: ExprParser = rulesFrom(tightest)

    lazy val tightest: ExprParser = log(group | value)("tightest")

    lazy val group: ExprParser = "(" ~> expr <~ ")"
    lazy val value: ExprParser = list | slot | out | pattern | number | str

    lazy val mathematica: ExprParser = expr
}

sealed trait ParseOutput {
    def toPrettyForm: String
}

case class ParseResult(expr: Expr) extends ParseOutput {
    def toPrettyForm = expr.toPrettyForm
}

case class ParseError(msg: String, file: String, pos: Position) extends ParseOutput {
    def message: String = {
        val fileName = (new java.io.File(file)).getName()
        s"$fileName:$pos failure: $msg\n\n${pos.longString}"
    }

    def toPrettyForm = message
}

object MathematicaParser {
    def parse(source: String, name: String = "<string>"): ParseOutput = {
        val parser = new MathematicaParser
        parser.parseAll(parser.mathematica, source) match {
            case parser.Success(expr, _) =>
                ParseResult(expr)
            case parser.NoSuccess(msg, ctx) =>
                ParseError(msg, name, ctx.pos)
        }
    }

    def parse(source: File): ParseOutput =
        parse(FileUtils.readFromFile(source), source.getPath)
}

object Main extends App {
    if (args.length > 0) {
        println(MathematicaParser.parse(args.mkString(" ")).toPrettyForm)
    } else {
        println("Nothing to do.")
    }
}
