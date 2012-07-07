package org.sympy

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

case class Eval(head: String, args: Expr*) extends Expr {
    def toFullForm = "%s[%s]".format(head, args.map(_.toFullForm).mkString(", "))
}

class MathematicaParser extends RegexParsers with PackratParsers {
    protected override val whiteSpace = """(\s|(?m)\(\*(\*(?!/)|[^*])*\*\))+""".r

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

    lazy val apply: PackratParser[Eval] = ident ~ "[" ~ repsep(expr, ",") ~ "]" ^^ {
        case head ~ "[" ~ args ~ "]" => Eval(head, args: _*)
    }

    lazy val list: PackratParser[Expr] = "{" ~ repsep(expr, ",") ~ "}" ^^ {
        case "{" ~ elems ~ "}" => Eval("List", elems: _*)
    }

    lazy val or: PackratParser[Expr] = (or | orExpr) ~ "||" ~ orExpr ^^ {
        case lhs ~ _ ~ rhs => Eval("Or", lhs, rhs)
    }
    lazy val orExpr: PackratParser[Expr] = and | not | eq | cmp | tightest

    lazy val and: PackratParser[Expr] = (and | andExpr) ~ "&&" ~ andExpr ^^ {
        case lhs ~ _ ~ rhs => Eval("And", lhs, rhs)
    }
    lazy val andExpr: PackratParser[Expr] = not | eq | cmp | tightest

    lazy val not: PackratParser[Expr] = "!" ~ (not | notExpr) ^^ {
        case _ ~ expr => Eval("Not", expr)
    }
    lazy val notExpr: PackratParser[Expr] = eq | cmp | tightest

    lazy val eq: PackratParser[Expr] =  (eq | eqExpr) ~ ("==" | "!=") ~ eqExpr ^^ {
        case lhs ~ "==" ~ rhs => Eval("Equal", lhs, rhs)
        case lhs ~ "!=" ~ rhs => Eval("Unequal", lhs, rhs)
    }
    lazy val eqExpr: PackratParser[Expr] = cmp | add | mul | exp | neg | tightest

    lazy val cmp: PackratParser[Expr] = (cmp | cmpExpr) ~ ("<" | ">") ~ cmpExpr ^^ {
        case lhs ~ "<" ~ rhs => Eval("Less", lhs, rhs)
        case lhs ~ ">" ~ rhs => Eval("Greater", lhs, rhs)
    }
    lazy val cmpExpr: PackratParser[Expr] = add | mul | exp | neg | tightest

    lazy val add: PackratParser[Expr] = (add | addExpr) ~ ("+" | "-") ~ addExpr ^^ {
        case lhs ~ "+" ~ rhs => Eval("Plus", lhs, rhs)
        case lhs ~ "-" ~ rhs => Eval("Subtract", lhs, rhs)
    }
    lazy val addExpr: PackratParser[Expr] = mul | exp | neg | tightest

    lazy val mul: PackratParser[Expr] = (mul | mulExpr) ~ ("*" | "/") ~ mulExpr ^^ {
        case lhs ~ "*" ~ rhs => Eval("Times", lhs, rhs)
        case lhs ~ "/" ~ rhs => Eval("Divide", lhs, rhs)
    }
    lazy val mulExpr: PackratParser[Expr] = exp | neg | tightest

    lazy val exp: PackratParser[Expr] = (exp | expExpr) ~ "^" ~ expExpr ^^ {
        case lhs ~ _ ~ rhs => Eval("Power", lhs, rhs)
    }
    lazy val expExpr: PackratParser[Expr] = neg | tightest

    lazy val neg: PackratParser[Expr] = "-" ~ (neg | negExpr) ^^ {
        case _ ~ expr => Eval("Neg", expr)
    }
    lazy val negExpr: PackratParser[Expr] = tightest

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
