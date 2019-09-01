package com.mjsonofharry.md5model.utils

import atto._, Atto._
import cats.implicits._
import java.text.DecimalFormat

object Utils {
  val QUOTE = '"'
  def quotate(inside: String): String = QUOTE + inside + QUOTE
  def keyValue[T](key: String, valueParser: Parser[T]) = string(key) ~ spaceChar ~> valueParser <~ whitespaces
  val quoteChar = char('"')
  val inQuotes = quoteChar ~> manyUntil(anyChar, quoteChar) map (_.mkString)
  def vector[T](n: Int, valueParser: Parser[T]) = manyN(n, valueParser <~ {spaceChar || whitespaces})
  val whitespaces = many(whitespace)
  private val decimalFormatter = new DecimalFormat("#.##########")
  def format(x: Double) = decimalFormatter.format(x)
}
