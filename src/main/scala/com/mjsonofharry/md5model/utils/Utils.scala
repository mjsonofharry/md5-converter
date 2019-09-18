package com.mjsonofharry.md5model.utils

import atto._, Atto._
import cats.implicits._
import java.text.DecimalFormat

object Utils {
  type Key = Double
  type JointName = String
  type AttributeName = String

  val QUOTE = '"'
  def quotate(inside: String): String = QUOTE + inside + QUOTE
  def keyValue[T](key: String, valueParser: Parser[T]) =
    string(key) ~ spaceChar ~> valueParser <~ whitespaces
  val quoteChar = char('"')
  val inQuotes = quoteChar ~> manyUntil(anyChar, quoteChar) map (_.mkString)
  def vector[T](n: Int, valueParser: Parser[T]) =
    manyN(n, valueParser <~ { spaceChar || whitespaces })
  val whitespaces = many(whitespace)

  private val decimalFormatter = new DecimalFormat("#.##########")
  def format(x: Double) = decimalFormatter.format(x)

  def padKeys(
      keys: List[Key],
      range: (Int, Int),
      padTo: Int
  ): List[Key] = {
    val (start, finish) = range
    val prependTo = start + keys.size
    val appendTo = finish
    val prepended = keys.reverse.padTo(start + keys.size, keys.head)
    val appended = prepended.reverse.padTo(finish, keys.last)
    appended
  }

  def spreadKeys(keys: List[Key], total: Int): List[Key] =
    (0 until total)
      .map(x => keys(((x.toDouble / total) * keys.size).toInt))
      .toList
}
