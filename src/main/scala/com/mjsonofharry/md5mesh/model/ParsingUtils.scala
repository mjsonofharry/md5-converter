package com.mjsonofharry.md5mesh.model

import atto._, Atto._
import cats.implicits._
import java.text.DecimalFormat

object ParsingUtils {
  val QUOTE = '"'
  def quotate(inside: String): String = QUOTE + inside + QUOTE
  val quoteChar = char('"')
  val inQuotes = quoteChar ~> manyUntil(anyChar, quoteChar) map (_.mkString)
  val whitespaces = many(whitespace)
  private val decimalFormatter = new DecimalFormat("#.##########")
  def format(x: Double) = decimalFormatter.format(x)
}
