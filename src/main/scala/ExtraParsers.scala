package org.sympy.parsing.mathematica

import scala.util.parsing.combinator.{Parsers,RegexParsers}
import scala.util.matching.Regex

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

trait ExtraRegexParsers { self: RegexParsers =>
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
}
