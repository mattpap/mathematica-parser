package org.sympy.parsing.mathematica

import java.io.File
import scala.io.Source
import scala.util.matching.Regex

import scala.util.parsing.combinator._
import scala.util.parsing.input.{Positional,Position}

sealed trait Expr {
    def toPrettyForm: String

    def apply(expr: Expr): Eval = Eval(this, expr)
}

case class Sym(name: String) extends Expr {
    def toPrettyForm = name
}

case class Num(value: String) extends Expr {
    def toPrettyForm = value
}

case class Str(value: String) extends Expr {
    def toPrettyForm = "\"%s\"".format(value.replace("\"", "\\\\\""))
}

sealed trait EvalLike extends Expr {
    val head: Expr
    val args: Seq[Expr]

    def toPrettyForm = {
        val head = this.head.toPrettyForm
        val args = this.args.map(_.toPrettyForm).mkString(", ")
        s"$head[$args]"
    }
}

case class Eval(head: Expr, args: Expr*) extends EvalLike {
    override def toString: String = {
        val name = productPrefix
        val args = this.args.map(_.toString).mkString(", ")
        s"$name($head, $args)"
    }
}

sealed trait Builtin extends EvalLike with scala.Product {
    override val head: Sym = Sym(productPrefix)

    override def toString: String = {
        val head = this.head.name
        val args = this.args.map(_.toString).mkString(", ")
        s"$head($args)"
    }
}

sealed trait Singleton extends Builtin {
    final val args: Seq[Expr] = Nil

    override def toPrettyForm = head.name
    override def toString = head.name
}

object Builtins {
    case class CompoundExpression(args: Expr*) extends Builtin
    case class Set(args: Expr*) extends Builtin
    case class SetDelayed(args: Expr*) extends Builtin
    case class AddTo(args: Expr*) extends Builtin
    case class SubtractFrom(args: Expr*) extends Builtin
    case class TimesBy(args: Expr*) extends Builtin
    case class DivideBy(args: Expr*) extends Builtin
    case class ReplaceAll(args: Expr*) extends Builtin
    case class Rule(args: Expr*) extends Builtin
    case class Or(args: Expr*) extends Builtin
    case class And(args: Expr*) extends Builtin
    case class Not(args: Expr*) extends Builtin
    case class SameQ(args: Expr*) extends Builtin
    case class Equal(args: Expr*) extends Builtin
    case class Unequal(args: Expr*) extends Builtin
    case class LessEqual(args: Expr*) extends Builtin
    case class Less(args: Expr*) extends Builtin
    case class GreaterEqual(args: Expr*) extends Builtin
    case class Greater(args: Expr*) extends Builtin
    case class Inequality(args: Expr*) extends Builtin
    case class Plus(args: Expr*) extends Builtin
    case class Subtract(args: Expr*) extends Builtin
    case class Times(args: Expr*) extends Builtin
    case class Divide(args: Expr*) extends Builtin
    case class Power(args: Expr*) extends Builtin
    case class List(args: Expr*) extends Builtin
    case class Span(args: Expr*) extends Builtin
    case class Part(args: Expr*) extends Builtin
    case class Exp(args: Expr*) extends Builtin
    case class Factorial(args: Expr*) extends Builtin
    case class Factorial2(args: Expr*) extends Builtin
    case class Out(args: Expr*) extends Builtin
    case class Slot(args: Expr*) extends Builtin
    case class SlotSequence(args: Expr*) extends Builtin
    case class Pattern(args: Expr*) extends Builtin
    case class Blank(args: Expr*) extends Builtin
    case class PatternTest(args: Expr*) extends Builtin
    case class Condition(args: Expr*) extends Builtin
    case class Increment(args: Expr*) extends Builtin
    case class PreIncrement(args: Expr*) extends Builtin
    case class Decrement(args: Expr*) extends Builtin
    case class PreDecrement(args: Expr*) extends Builtin
    case class Derivative(args: Expr*) extends Builtin
    case class Dot(args: Expr*) extends Builtin
    case class Function(args: Expr*) extends Builtin
}

object Singletons {
    case object All extends Singleton
    case object Null extends Singleton
    case object True extends Singleton
    case object False extends Singleton
}

object Implicits {
    implicit def intToNum(value: Int): Expr = Num(value.toString)
    implicit def doubleToNum(value: Double): Expr = Num(value.toString)
    implicit def stringToStr(value: String): Expr = Str(value)
    implicit def symbolToSym(value: Symbol): Expr = Sym(value.name)
    implicit def booleanToBoolean(value: Boolean): Expr =
        if (value) Singletons.True else Singletons.False
}

trait ExtraParsers { self: Parsers =>
    def notFollowedBy[T](p: => Parser[T], elems: Elem*): Parser[T] = Parser { in =>
        p(in) match {
            case success @ Success(_, rest) =>
                if (rest.atEnd) success
                else {
                    elems.find(rest.first == _) match {
                        case None =>
                            success
                        case Some(elem) =>
                            Failure(s"`$elem' not allowed in this context", rest)
                    }
                }
            case error => error
        }
    }

    protected val loggingEnabled: Boolean = false

    override def log[T](p: => Parser[T])(name: String): Parser[T] = Parser { in =>
        if (loggingEnabled) println(s"trying $name at $in")
        val result = p(in)
        if (loggingEnabled) println(s"$name --> $result")
        result
    }
}

class MathematicaParser extends RegexParsers with PackratParsers with ExtraParsers {
    protected override val whiteSpace = """(\s|(?m)\(\*(\*(?!/)|[^*])*\*\))+""".r

    def regexMatch(r: Regex): Parser[Regex.Match] = new Parser[Regex.Match] {
        def apply(in: Input) = {
            val source = in.source
            val offset = in.offset
            val start = handleWhiteSpace(source, offset)
            (r findPrefixMatchOf (source.subSequence(start, source.length))) match {
                case Some(matched) =>
                    Success(matched, in.drop(start + matched.end - offset))
                case None =>
                    val found = if (start == source.length()) "end of source" else "`" + source.charAt(start) + "'"
                    Failure("string matching regex `" + r + "' expected but " + found + " found", in.drop(start - offset))
            }
        }
    }

    import Implicits._

    protected val name = "[a-zA-Z][a-zA-Z0-9]*"

    lazy val ident: PackratParser[String] = regex(name.r)

    lazy val pattern: PackratParser[Expr] =
        log(symbolBlankWithHead | symbolBlank | symbol | blankWithHead | blank)("pattern")

    lazy val blank: PackratParser[Expr] = "_" ^^ {
        case _ => Builtins.Blank()
    }
    lazy val blankWithHead: PackratParser[Expr] = regexMatch(s"_($name)".r) ^^ {
        case Regex.Groups(head) => Builtins.Blank(Sym(head))
    }
    lazy val symbol: PackratParser[Expr] = regexMatch(s"($name)".r) ^^ {
        case Regex.Groups(name) => Sym(name)
    }
    lazy val symbolBlank: PackratParser[Expr] = regexMatch(s"($name)_".r) ^^ {
        case Regex.Groups(name) => Builtins.Pattern(Sym(name), Builtins.Blank())
    }
    lazy val symbolBlankWithHead: PackratParser[Expr] = regexMatch(s"($name)_($name)".r) ^^ {
        case Regex.Groups(name, head) => Builtins.Pattern(Sym(name), Builtins.Blank(Sym(head)))
    }

    lazy val number: PackratParser[Num] = log("""(\d+\.\d*|\d*\.\d+|\d+)([eE][+-]?\d+)?""".r)("number") ^^ {
        case value => Num(value)
    }

    lazy val str: PackratParser[Str] = log(("\"" + """(\\.|[^"])*""" + "\"").r)("str") ^^ {
        case value => Str(value.stripPrefix("\"").stripSuffix("\"").replace("\\\\\"", "\""))
    }

    lazy val out: PackratParser[Expr] = outNumber | outClassic
    lazy val outNumber: PackratParser[Expr] = "%\\d+".r ^^ {
        case value => Builtins.Out(Num(value.stripPrefix("%")))
    }
    lazy val outClassic: PackratParser[Expr] = "%+".r ^^ {
        case "%"   => Builtins.Out()
        case value => Builtins.Out(Num(s"-${value.length}"))
    }

    lazy val slot: PackratParser[Expr] = regexMatch("(#{1,2})(\\d*)".r) ^^ {
        case Regex.Groups("#",  "")    => Builtins.Slot(1)
        case Regex.Groups("#",  index) => Builtins.Slot(index.toInt)
        case Regex.Groups("##", "")    => Builtins.SlotSequence(1)
        case Regex.Groups("##", index) => Builtins.SlotSequence(index.toInt)
    }

    lazy val list: PackratParser[Expr] = "{" ~ repsep(expr, ",") ~ "}" ^^ {
        case "{" ~ elems ~ "}" => Builtins.List(elems: _*)
    }

    lazy val expr: PackratParser[Expr] =
        // Precedence in increasing order:
        compound  | // infix, flat  :  ;
        assign    | // infix, right :  = := += -= *= /=
        func      | // postfix      :  &
        replace   | // infix, left  :  /.
        rule      | // infix, right :  ->
        cond      | // infix, left  :  /;
        or        | // infix, flat  :  ||
        and       | // infix, flat  :  &&
        not       | // prefix       :  !
        same      | // infix, flat  :  ===
        cmp       | // infix, flat  :  == != <= < >= >
        span      | // infix, none  :  ;;
        add       | // infix, flat  :  + -
        mul       | // infix, flat  :  * /
        dot       | // infix, flat  :  .
        neg       | // prefix       :  -
        exp       | // infix, right :  ^
        deriv     | // postifx      :  '
        eval      | // postfix      :  []
        dec       | // postfix      :  --
        inc       | // postfix      :  ++
        factorial | // postfix      :  ! !!
        part      | // postfix      :  [[]]
        test      | // infix, none  :  ?
        tightest

    lazy val compound: PackratParser[Expr] = log(complexCompound | simpleCompound)("compound")
    lazy val semicolon: PackratParser[String] = notFollowedBy(";", ';')
    lazy val complexCompound: PackratParser[Expr] = compoundExpr ~ semicolon ~ rep1sep(compoundExpr, semicolon) ~ opt(semicolon) ^^ {
        case elem ~ _ ~ elems ~ None =>
            Builtins.CompoundExpression(elem :: elems: _*)
        case elem ~ _ ~ elems ~ _ =>
            Builtins.CompoundExpression((elem :: elems) :+ Singletons.Null: _*)
    }
    lazy val simpleCompound: PackratParser[Expr] = compoundExpr ~ semicolon ^^ {
        case elem ~ _ => Builtins.CompoundExpression(elem, Singletons.Null)
    }
    lazy val compoundExpr: PackratParser[Expr] =
        assign | func | replace | rule | cond | or | and | not | same | cmp | span | add | mul | dot | neg | exp | deriv | eval | dec | inc | factorial | part | test | tightest

    lazy val assign: PackratParser[Expr] = log(assignExpr ~ ("=" | ":=" | "+=" | "-=" | "*=" | "/=") ~ (assign | assignExpr))("assign") ^^ {
        case lhs ~  "=" ~ rhs => Builtins.Set(lhs, rhs)
        case lhs ~ ":=" ~ rhs => Builtins.SetDelayed(lhs, rhs)
        case lhs ~ "+=" ~ rhs => Builtins.AddTo(lhs, rhs)
        case lhs ~ "-=" ~ rhs => Builtins.SubtractFrom(lhs, rhs)
        case lhs ~ "*=" ~ rhs => Builtins.TimesBy(lhs, rhs)
        case lhs ~ "/=" ~ rhs => Builtins.DivideBy(lhs, rhs)
    }
    lazy val assignExpr: PackratParser[Expr] =
        func | replace | rule | cond | or | and | not | same | cmp | span | add | mul | dot | neg | exp | deriv | eval | dec | inc | factorial | part | test | tightest

    lazy val func: PackratParser[Expr] = log((func | funcExpr) ~ notFollowedBy("&", '&'))("func") ^^ {
        case expr ~ _ => Builtins.Function(expr)
    }
    lazy val funcExpr: PackratParser[Expr] =
        replace | rule | cond | or | and | not | same | cmp | span | add | mul | dot | neg | exp | deriv | eval | dec | inc | factorial | part | test | tightest

    lazy val replace: PackratParser[Expr] = log((replace | replaceExpr) ~ "/." ~ replaceExpr)("replace") ^^ {
        case lhs ~ _ ~ rhs => Builtins.ReplaceAll(lhs, rhs)
    }
    lazy val replaceExpr: PackratParser[Expr] =
        rule | cond | or | and | not | same | cmp | span | add | mul | dot | neg | exp | deriv | eval | dec | inc | factorial | part | test | tightest

    lazy val rule: PackratParser[Expr] = log(ruleExpr ~ "->" ~ (rule | ruleExpr))("rule") ^^ {
        case lhs ~ _ ~ rhs => Builtins.Rule(lhs, rhs)
    }
    lazy val ruleExpr: PackratParser[Expr] =
        cond | or | and | not | same | cmp | span | add | mul | dot | neg | exp | deriv | eval | dec | inc | factorial | part | test | tightest

    lazy val cond: PackratParser[Expr] = log((cond | condExpr) ~ "/;" ~ condExpr)("cond") ^^ {
        case lhs ~ _ ~ rhs => Builtins.Condition(lhs, rhs)
    }
    lazy val condExpr: PackratParser[Expr] =
        or | and | not | same | cmp | span | add | mul | dot | neg | exp | deriv | eval | dec | inc | factorial | part | test | tightest

    lazy val or: PackratParser[Expr] = log(orExpr ~ rep1("||" ~> orExpr))("or") ^^ {
        case head ~ tail => Builtins.Or(head :: tail: _*)
    }
    lazy val orExpr: PackratParser[Expr] =
        and | not | same | cmp | span | add | mul | dot | neg | exp | deriv | eval | dec | inc | factorial | part | test | tightest

    lazy val and: PackratParser[Expr] = log(andExpr ~ rep1("&&" ~> andExpr))("and") ^^ {
        case head ~ tail => Builtins.And(head :: tail: _*)
    }
    lazy val andExpr: PackratParser[Expr] =
        not | same | cmp | span | add | mul | dot | neg | exp | deriv | eval | dec | inc | factorial | part | test | tightest

    lazy val not: PackratParser[Expr] = log(notFollowedBy("!", '=') ~ (not | notExpr))("not") ^^ {
        case _ ~ expr => Builtins.Not(expr)
    }
    lazy val notExpr: PackratParser[Expr] =
        same | cmp | span | add | mul | dot | neg | exp | deriv | eval | dec | inc | factorial | part | test | tightest

    lazy val same: PackratParser[Expr] = log(sameExpr ~ rep1("===" ~> (not | sameExpr)))("same") ^^ {
        case head ~ tail => Builtins.SameQ(head :: tail: _*)
    }
    lazy val sameExpr: PackratParser[Expr] =
        cmp | span | add | mul | dot | neg | exp | deriv | eval | dec | inc | factorial | part | test | tightest

    protected def evalCmpOp(op: String, args: Expr*): Builtin = (op match {
        case "==" => Builtins.Equal
        case "!=" => Builtins.Unequal
        case "<=" => Builtins.LessEqual
        case "<"  => Builtins.Less
        case ">=" => Builtins.GreaterEqual
        case ">"  => Builtins.Greater
    })(args: _*)

    lazy val cmp: PackratParser[Expr] = log(cmpExpr ~ rep1(("==" | "!="  | "<=" | "<" | ">=" | ">") ~ cmpExpr))("cmp") ^^ {
        case head ~ tail =>
            tail.collect { case op ~ _ => op }.distinct match {
                case op :: Nil =>
                    val args = head :: tail.collect {
                        case _ ~ expr => expr
                    }
                    evalCmpOp(op, args: _*)
                case _ =>
                    val args = head :: tail.flatMap {
                        case op ~ expr => Seq(evalCmpOp(op), expr)
                    }
                    Builtins.Inequality(args: _*)
            }
    }
    lazy val cmpExpr: PackratParser[Expr] =
        span | add | mul | dot | neg | exp | deriv | eval | dec | inc | factorial | part | test | tightest

    lazy val span: PackratParser[Expr] = log(spanParser)("span")
    lazy val spanParser: PackratParser[Expr] =
        spanExpr ~ ";;" ~ spanExpr ~ ";;" ~ spanExpr ^^ { case i ~ _ ~ j ~ _ ~ k => Builtins.Span(i, j, k)              } |
        spanExpr ~ ";;"            ~ ";;" ~ spanExpr ^^ { case i ~ _     ~ _ ~ k => Builtins.Span(i, Singletons.All, k) } |
                   ";;" ~ spanExpr ~ ";;" ~ spanExpr ^^ { case     _ ~ j ~ _ ~ k => Builtins.Span(1, j, k)              } |
                   ";;"            ~ ";;" ~ spanExpr ^^ { case     _     ~ _ ~ k => Builtins.Span(1, Singletons.All, k) } |
        spanExpr ~ ";;" ~ spanExpr                   ^^ { case i ~ _ ~ j         => Builtins.Span(i, j)                 } |
        spanExpr ~ ";;"                              ^^ { case i ~ _             => Builtins.Span(i, Singletons.All)    } |
                   ";;" ~ spanExpr                   ^^ { case     _ ~ j         => Builtins.Span(1, j)                 } |
                   ";;"                              ^^ { case     _             => Builtins.Span(1, Singletons.All)    }
    lazy val spanExpr: PackratParser[Expr] = add | mul | dot | neg | exp | deriv | eval | dec | inc | factorial | part | test | tightest

    lazy val add: PackratParser[Expr] = log(addExpr ~ rep1(("+" | "-") ~ addExpr))("add") ^^ {
        case head ~ tail =>
            val args = head :: tail.map {
                case "+" ~ expr       => expr
                case "-" ~ Num(value) => Num(s"-$value")
                case "-" ~ expr       => Builtins.Times(-1, expr)
            }
            Builtins.Plus(args: _*)
    }
    lazy val addExpr: PackratParser[Expr] =
        mul | dot | neg | exp | deriv | eval | dec | inc | factorial | part | test | tightest

    lazy val mul: PackratParser[Expr] = log(mulImplied | mulExplicit)("mul")
    lazy val mulImplied: PackratParser[Expr] = mulExpr ~ rep1(mulImpliedRhsExpr) ^^ {
        case head ~ tail =>
            val args = head :: tail
            Builtins.Times(args: _*)
    }
    lazy val mulExplicit: PackratParser[Expr] = mulExpr ~ rep1(("*" | notFollowedBy("/", ';', '.')) ~ mulExpr) ^^ {
        case head ~ tail =>
            val args = head :: tail.map {
                case "*" ~ expr => expr
                case "/" ~ expr => Builtins.Power(expr, -1)
            }
            Builtins.Times(args: _*)
    }
    lazy val mulImpliedRhsExpr: PackratParser[Expr] =
        /*not | */dot | /*neg | */ exp | deriv | eval | dec | inc | factorial | part | test | tightest
    lazy val mulExpr: PackratParser[Expr] =
        /*not | */dot |   neg |    exp | deriv | eval | dec | inc | factorial | part | test | tightest

    lazy val dot: PackratParser[Expr] = log(dotExpr ~ rep1("." ~> dotExpr))("dot") ^^ {
        case head ~ tail => Builtins.Dot(head :: tail: _*)
    }
    lazy val dotExpr: PackratParser[Expr] = exp | deriv | eval | dec | inc | factorial | part | test | tightest

    lazy val neg: PackratParser[Expr] = log("-" ~ (neg | negExpr))("neg") ^^ {
        case _ ~ Num(value) => Num(s"-$value")
        case _ ~ expr => Builtins.Times(-1, expr)
    }
    lazy val negExpr: PackratParser[Expr] =
        exp | deriv | eval | dec | inc | factorial | part | test | tightest

    lazy val exp: PackratParser[Expr] = log(expLhsExpr ~ "^" ~ (exp | expRhsExpr))("exp") ^^ {
        case lhs ~ _ ~ rhs => Builtins.Power(lhs, rhs)
    }
    lazy val expLhsExpr: PackratParser[Expr] = deriv | eval | dec | inc | factorial | part | test | tightest
    lazy val expRhsExpr: PackratParser[Expr] = neg | expLhsExpr

    lazy val deriv: PackratParser[Expr] = log((deriv | derivExpr) ~ "'+".r)("deriv") ^^ {
        case expr ~ ticks => Eval(Builtins.Derivative(ticks.length), expr)
    }
    lazy val derivExpr: PackratParser[Expr] = eval | dec | inc | factorial | part | test | tightest

    lazy val eval: PackratParser[Expr] = log((eval | evalExpr) ~ (notFollowedBy("[", '[') ~> repsep(expr, ",") <~ "]"))("eval") ^^ {
        // TODO: this has to be automated (e.g. with reflection)
        case Sym("Exp") ~ args => Builtins.Exp(args: _*)
        case head       ~ args => Eval(head, args: _*)
    }
    lazy val evalExpr: PackratParser[Expr] = dec | inc | factorial | part | test | tightest

    lazy val inc: PackratParser[Expr] = failure("inc") // log(failure("inc"))("inc")
    lazy val dec: PackratParser[Expr] = failure("dec") // log(failure("dec"))("inc")

    lazy val factorial: PackratParser[Expr] = log((factorial | factorialExpr) ~ ("!!" | notFollowedBy("!", '=')))("factorial") ^^ {
        case expr ~ "!"  => Builtins.Factorial(expr)
        case expr ~ "!!" => Builtins.Factorial2(expr)
    }
    lazy val factorialExpr: PackratParser[Expr] = part | test | tightest

    lazy val part: PackratParser[Expr] = log((part | partExpr) ~ "[[" ~ repsep(expr, ",") ~ "]]")("part") ^^ {
        case expr ~ _ ~ indices ~ _ => Builtins.Part(expr :: indices: _*)
    }
    lazy val partExpr: PackratParser[Expr] = test | tightest

    lazy val test: PackratParser[Expr] = log(testExpr ~ "?" ~ testExpr)("test") ^^ {
        case lhs ~ _ ~ rhs => Builtins.PatternTest(lhs, rhs)
    }
    lazy val testExpr: PackratParser[Expr] = tightest

    lazy val tightest: PackratParser[Expr] = log(group | value)("tightest")

    lazy val group: PackratParser[Expr] = "(" ~> expr <~ ")"
    lazy val value: PackratParser[Expr] = list | slot | out | pattern | number | str

    lazy val mathematica: PackratParser[Expr] = expr
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

object FileUtils {
    def readFromFile(file: File): String =
        Source.fromFile(file).mkString("")
}

object TimeUtils {
    def timed[T](code: => T): (T, Long) = {
        val start = System.currentTimeMillis
        val result = code
        val end = System.currentTimeMillis
        (result, end - start)
    }
}

object Main extends App {
    if (args.length > 0) {
        println(MathematicaParser.parse(args.mkString(" ")).toPrettyForm)
    } else {
        println("Nothing to do.")
    }
}
