package com.mjsonofharry.md5model.utils

import atto._, Atto._
import cats.implicits._
import java.text.DecimalFormat
import java.util.Locale

object Utils {
  val QUOTE = '"'
  def quotate(inside: String): String = QUOTE + inside + QUOTE
  def keyValue[T](key: String, valueParser: Parser[T]) =
    string(key) ~ spaceChar ~> valueParser <~ whitespaces
  val quoteChar = char('"')
  val inQuotes = quoteChar ~> manyUntil(anyChar, quoteChar) map (_.mkString)
  def vector[T](n: Int, valueParser: Parser[T]) =
    manyN(n, valueParser <~ { spaceChar || whitespaces })
  val whitespaces = many(whitespace)


  def format(x: Double): String = x.toLong match {
    case x1 if x1 == x => "%d".formatLocal(Locale.ROOT, x1)
    case _ => {
      val x1 =
        BigDecimal(x).setScale(10, BigDecimal.RoundingMode.HALF_UP).toDouble
      "%s".formatLocal(Locale.ROOT, x1)
    }
  }
}
