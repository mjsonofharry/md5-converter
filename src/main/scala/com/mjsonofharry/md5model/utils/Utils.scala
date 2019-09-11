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

  val powers: Stream[Int] = Stream.from(2).scanLeft(1)((x, _) => x * 2)
  def bin2int(bits: String): Int =
    bits.toList
      .map(_.asDigit)
      .zip(powers.take(bits.size).toList.reverse)
      .map(
        xe =>
          xe match {
            case (1, e) => e
            case (0, _) => 0
            case (x, _) =>
              throw new NumberFormatException(s"Cannot convert ${x} into a bit")
          }
      )
      .reduce(_ + _)
}
