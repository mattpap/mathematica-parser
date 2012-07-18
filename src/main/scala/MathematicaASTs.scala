package org.sympy.parsing.mathematica

sealed trait Expr {
    def toPrettyForm: String

    def apply(exprs: Expr*): Eval = Eval(this, exprs: _*)
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

case class Eval(head: Expr, args: Expr*) extends Expr {
    def toPrettyForm = {
        val head = this.head.toPrettyForm
        val args = this.args.map(_.toPrettyForm).mkString(", ")
        s"$head[$args]"
    }

    override def toString: String = {
        val args = this.args.map(_.toString).mkString(", ")
        s"Eval($head, $args)"
    }
}

object Implicits {
    implicit def intToExpr(value: Int): Expr = Num(value.toString)
    implicit def doubleToExpr(value: Double): Expr = Num(value.toString)
    implicit def stringToExpr(value: String): Expr = Str(value)
    implicit def symbolToExpr(value: Symbol): Expr = Sym(value.name)
    implicit def booleanToExpr(value: Boolean): Expr = if (value) Sym("True") else Sym("False")
}
