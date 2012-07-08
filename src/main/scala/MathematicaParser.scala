package org.sympy.parsing.mathematica

import java.io.File
import scala.io.Source

import scala.util.parsing.combinator._
import scala.util.parsing.input.{Positional,Position}

sealed trait Expr {
    def toPrettyForm: String
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
    val head: String
    val args: Seq[Expr]

    def toPrettyForm = {
        val args = this.args.map(_.toPrettyForm).mkString(", ")
        s"$head[$args]"
    }
}

case class Eval(head: String, args: Expr*) extends EvalLike

sealed trait BuiltinEval extends EvalLike with Product {
    val head = productPrefix
}

case class Plus(args: Expr*) extends BuiltinEval
case class Subtract(args: Expr*) extends BuiltinEval
case class Times(args: Expr*) extends BuiltinEval
case class Divide(args: Expr*) extends BuiltinEval
case class Power(args: Expr*) extends BuiltinEval
case class Neg(args: Expr*) extends BuiltinEval
case class List(args: Expr*) extends BuiltinEval
case class Or(args: Expr*) extends BuiltinEval
case class And(args: Expr*) extends BuiltinEval
case class Not(args: Expr*) extends BuiltinEval
case class Equal(args: Expr*) extends BuiltinEval
case class Unequal(args: Expr*) extends BuiltinEval
case class Less(args: Expr*) extends BuiltinEval
case class Greater(args: Expr*) extends BuiltinEval
case class Span(args: Expr*) extends BuiltinEval
case class Part(args: Expr*) extends BuiltinEval

sealed trait Singleton extends BuiltinEval {
    final val args: Seq[Expr] = Nil

    override def toPrettyForm = head
}

case object All extends Singleton
case object True extends Singleton
case object False extends Singleton

object MathematicaImplicits {
    implicit def intToNum(value: Int): Expr = Num(value.toString)
    implicit def doubleToNum(value: Double): Expr = Num(value.toString)
    implicit def stringToStr(value: String): Expr = Str(value)
    implicit def symbolToSym(value: Symbol): Expr = Sym(value.name)
    implicit def booleanToBoolean(value: Boolean): Expr = if (value) True else False
}

class MathematicaParser extends RegexParsers with PackratParsers {
    protected override val whiteSpace = """(\s|(?m)\(\*(\*(?!/)|[^*])*\*\))+""".r

    import MathematicaImplicits._

    lazy val ident: PackratParser[String] = regex("""[a-zA-Z][a-zA-Z0-9]*""".r)

    lazy val symbol: PackratParser[Sym] = ident ^^ {
        case name => Sym(name)
    }

    lazy val number: PackratParser[Num] = """-?(\d+\.\d*|\d*\.\d+|\d+)([eE][+-]?\d+)?""".r ^^ {
        case value => Num(value)
    }

    lazy val str: PackratParser[Str] = ("\"" + """(\\.|[^"])*""" + "\"").r ^^ {
        case value => Str(value.stripPrefix("\"").stripSuffix("\"").replace("\\\\\"", "\""))
    }

    lazy val apply: PackratParser[Expr] = ident ~ "[" ~ repsep(expr, ",") ~ "]" ^^ {
        case head ~ "[" ~ args ~ "]" => Eval(head, args: _*)
    }

    lazy val list: PackratParser[Expr] = "{" ~ repsep(expr, ",") ~ "}" ^^ {
        case "{" ~ elems ~ "}" => List(elems: _*)
    }

    lazy val or: PackratParser[Expr] = (or | orExpr) ~ "||" ~ orExpr ^^ {
        case lhs ~ _ ~ rhs => Or(lhs, rhs)
    }
    lazy val orExpr: PackratParser[Expr] = and | not | eq | cmp | tightest

    lazy val and: PackratParser[Expr] = (and | andExpr) ~ "&&" ~ andExpr ^^ {
        case lhs ~ _ ~ rhs => And(lhs, rhs)
    }
    lazy val andExpr: PackratParser[Expr] = not | eq | cmp | tightest

    lazy val not: PackratParser[Expr] = "!" ~ (not | notExpr) ^^ {
        case _ ~ expr => Not(expr)
    }
    lazy val notExpr: PackratParser[Expr] = eq | cmp | tightest

    lazy val eq: PackratParser[Expr] =  (eq | eqExpr) ~ ("==" | "!=") ~ eqExpr ^^ {
        case lhs ~ "==" ~ rhs => Equal(lhs, rhs)
        case lhs ~ "!=" ~ rhs => Unequal(lhs, rhs)
    }
    lazy val eqExpr: PackratParser[Expr] = cmp | add | mul | exp | neg | tightest

    lazy val cmp: PackratParser[Expr] = (cmp | cmpExpr) ~ ("<" | ">") ~ cmpExpr ^^ {
        case lhs ~ "<" ~ rhs => Less(lhs, rhs)
        case lhs ~ ">" ~ rhs => Greater(lhs, rhs)
    }
    lazy val cmpExpr: PackratParser[Expr] = add | mul | exp | neg | tightest

    lazy val add: PackratParser[Expr] = (add | addExpr) ~ ("+" | "-") ~ addExpr ^^ {
        case lhs ~ "+" ~ rhs => Plus(lhs, rhs)
        case lhs ~ "-" ~ rhs => Subtract(lhs, rhs)
    }
    lazy val addExpr: PackratParser[Expr] = mul | exp | neg | tightest

    lazy val mul: PackratParser[Expr] = (mul | mulExpr) ~ ("*" | "/") ~ mulExpr ^^ {
        case lhs ~ "*" ~ rhs => Times(lhs, rhs)
        case lhs ~ "/" ~ rhs => Divide(lhs, rhs)
    }
    lazy val mulExpr: PackratParser[Expr] = exp | neg | tightest

    lazy val exp: PackratParser[Expr] = (exp | expExpr) ~ "^" ~ expExpr ^^ {
        case lhs ~ _ ~ rhs => Power(lhs, rhs)
    }
    lazy val expExpr: PackratParser[Expr] = neg | tightest

    lazy val neg: PackratParser[Expr] = "-" ~ (neg | negExpr) ^^ {
        case _ ~ expr => Neg(expr)
    }
    lazy val negExpr: PackratParser[Expr] = tightest

    lazy val part: PackratParser[Expr] = (part | partExpr) ~ "[[" ~ repsep(expr, ",") ~ "]]" ^^ {
        case expr ~ _ ~ indices ~ _ => Part(indices: _*)
    }
    lazy val partExpr: PackratParser[Expr] = tightest

    lazy val span: PackratParser[Expr] =
        expr ~ ";;" ~ expr               ^^ { case i ~ _ ~ j         => Span(i, j) }      |
        expr ~ ";;"                      ^^ { case i ~ _             => Span(i, All) }    |
               ";;" ~ expr               ^^ { case     _ ~ j         => Span(1, j) }      |
               ";;"                      ^^ { case     _             => Span(1, All) }    |
        expr ~ ";;" ~ expr ~ ";;" ~ expr ^^ { case i ~ _ ~ j ~ _ ~ k => Span(i, j, k) }   |
        expr ~ ";;"        ~ ";;" ~ expr ^^ { case i ~ _     ~ _ ~ k => Span(i, All, k) } |
               ";;" ~ expr ~ ";;" ~ expr ^^ { case     _ ~ j ~ _ ~ k => Span(1, j, k) }   |
               ";;" ~        ";;" ~ expr ^^ { case     _     ~ _ ~ k => Span(1, All, k) }

    lazy val tightest: PackratParser[Expr] = group | value

    lazy val group: PackratParser[Expr] = "(" ~> expr <~ ")"
    lazy val value: PackratParser[Expr] = apply | list | symbol | number | str

    lazy val expr: PackratParser[Expr] = or | and | not | eq | cmp | add | mul | exp | neg | tightest

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
        Source.fromFile(file).mkString("\n")
}

object Main extends App {
    if (args.length > 0) {
        println(MathematicaParser.parse(args.mkString(" ")).toPrettyForm)
    } else {
        println("Nothing to do.")
    }
}
