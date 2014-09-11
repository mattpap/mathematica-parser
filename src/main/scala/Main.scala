package org.refptr.parsing.mathematica

import java.io.File
import scopt.{OptionParser,Read}

object Main extends App {
    sealed trait Form {
        def toForm(expr: Expr): String
    }
    case object FullForm extends Form {
        def toForm(expr: Expr) = expr.toFullForm
    }
    case object LispForm extends Form {
        def toForm(expr: Expr) = expr.toLispForm
    }

    implicit val formReads: Read[Form] = Read.reads {
        case "full" => FullForm
        case "lisp" => LispForm
        case string => throw new IllegalArgumentException(s"'$string' is not a valid form name.")
    }

    case class Config(
        form: Form = FullForm,
        files: List[File] = Nil,
        exprs: List[String] = Nil)

    val config: Config = {
        val parser = new scopt.OptionParser[Config]("mathematica-parser") {
            opt[Form]('F', "form")
                .action { (form, config) => config.copy(form=form) }
                .text("output form: full, lisp")

            opt[File]('f', "file")
                .unbounded()
                .optional()
                .action { (file, config) => config.copy(files = config.files :+ file) }
                .text("input file")

            opt[String]('e', "expr")
                .unbounded()
                .optional()
                .action { (expr, config) => config.copy(exprs = config.exprs :+ expr) }
                .text("input expression")

            help("help") text("prints this usage text")
        }

        parser.parse(args, Config()) getOrElse { sys.exit(1) }
    }

    if (config.files ++ config.exprs isEmpty) {
        sys.error("Noting to do.")
    } else {
        val files = config.files.map { case file =>
            MathematicaParser.parse(file)
        }

        val exprs = config.exprs.zipWithIndex.map { case (expr, i) =>
            MathematicaParser.parse(expr, s"<expr_$i>")
        }

        (files ++ exprs).map {
            case ParseResult(expr)   => config.form.toForm(expr)
            case (error: ParseError) => error.message
        }.foreach(println)
    }
}
