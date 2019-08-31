package com.mjsonofharry.md5mesh.model

import atto._, Atto._
import cats.implicits._

object ParsingUtils {
  val lcurl = char('{')
  val rcurl = char('}')
  val quote = char('"')
  val inQuotes = quote ~> manyUntil(anyChar, quote) map (_.mkString)
  val whitespaces = many(whitespace)
}
