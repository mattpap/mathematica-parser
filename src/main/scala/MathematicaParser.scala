package org.sympy.parsing.mathematica

import java.io.File
import scala.io.Source

import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional

sealed trait Expr {
    def toFullForm: String
}

case class Sym(name: String) extends Expr {
    def toFullForm = name
}

case class Num(value: String) extends Expr {
    def toFullForm = value
}

case class Str(value: String) extends Expr {
    def toFullForm = "\"%s\"".format(value.replace("\"", "\\\\\""))
}

sealed trait EvalLike extends Expr {
    val head: String
    val args: Seq[Expr]

    def toFullForm = "%s[%s]".format(head, args.map(_.toFullForm).mkString(", "))
}

case class Eval(head: String, args: Expr*) extends EvalLike

case class Plus(args: Expr*) extends EvalLike {
    val head = "Plus"
}
case class Subtract(args: Expr*) extends EvalLike {
    val head = "Subtract"
}
case class Times(args: Expr*) extends EvalLike {
    val head = "Times"
}
case class Divide(args: Expr*) extends EvalLike {
    val head = "Divide"
}
case class Power(args: Expr*) extends EvalLike {
    val head = "Power"
}
case class Neg(args: Expr*) extends EvalLike {
    val head = "Neg"
}
case class List(args: Expr*) extends EvalLike {
    val head = "List"
}
case class Or(args: Expr*) extends EvalLike {
    val head = "Or"
}
case class And(args: Expr*) extends EvalLike {
    val head = "And"
}
case class Not(args: Expr*) extends EvalLike {
    val head = "Not"
}
case class Equal(args: Expr*) extends EvalLike {
    val head = "Equal"
}
case class Unequal(args: Expr*) extends EvalLike {
    val head = "Unequal"
}
case class Less(args: Expr*) extends EvalLike {
    val head = "Less"
}
case class Greater(args: Expr*) extends EvalLike {
    val head = "Greater"
}
case class Span(args: Expr*) extends EvalLike {
    val head = "Span"
}
case class Part(args: Expr*) extends EvalLike {
    val head = "Part"
}

sealed trait Singleton extends EvalLike {
    val args: Seq[Expr] = Nil
}

case object All extends Singleton {
    val head = "All"
}

object MathematicaImplicits {
    implicit def intToNum(value: Int): Num = Num(value.toString)
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

object MathematicaParser {
    def parse(source: String, name: String = "<string>"): Option[Expr] = {
        val parser = new MathematicaParser
        parser.parseAll(parser.mathematica, source) match {
            case parser.Success(exprs, _) =>
                Some(exprs)
            case parser.NoSuccess(msg, ctx) =>
                println(msg)
                None
        }
    }

    def parse(source: File): Option[Expr] =
        parse(FileUtils.readFromFile(source), source.getPath)
}

object FileUtils {
    def readFromFile(file: File): String =
        Source.fromFile(file).mkString("")
}

object Main extends App {
    if (args.length > 0) {
        val input = args.mkString(" ")
        val output = MathematicaParser.parse(input)
        output.foreach(output => println(output.toFullForm))
    } else {
        println("Nothing to do.")
    }
}
