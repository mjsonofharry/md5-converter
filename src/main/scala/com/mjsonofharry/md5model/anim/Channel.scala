package com.mjsonofharry.md5model.anim

import atto._, Atto._
import cats.implicits._

import com.mjsonofharry.md5model.utils.Utils._

case class Channel(
    index: Int,
    joint: String,
    attribute: String,
    starttime: Double,
    endtime: Double,
    framerate: Double,
    strings: Int,
    range: (Int, Int),
    keys: List[Double]
)

object Channel {
  val parser: Parser[Channel] = for {
    index <- keyValue("channel", int) <~ char('{') ~ whitespaces
    joint <- keyValue("joint", inQuotes)
    attribute <- keyValue("attribute", inQuotes)
    starttime <- keyValue("starttime", double)
    endtime <- keyValue("endtime", double)
    framerate <- keyValue("framerate", double)
    strings <- keyValue("strings", int)
    range <- keyValue("range", { int <~ spaceChar } ~ int)
    numkeys <- keyValue("keys", int)
    keys <- manyUntil(double <~ whitespaces, char('}'))
  } yield
    Channel(
      index,
      joint,
      attribute,
      starttime,
      endtime,
      framerate,
      strings,
      range,
      keys
    )
}
