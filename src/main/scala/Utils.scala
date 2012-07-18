package org.sympy.parsing.mathematica

import java.io.File
import scala.io.Source

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
