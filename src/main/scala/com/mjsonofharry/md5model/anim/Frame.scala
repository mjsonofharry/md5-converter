package com.mjsonofharry.md5model.anim

case class Frame(index: Int, values: List[List[Double]])

object Frame {
  def apply(index: Int, values: List[List[Double]]): Frame =
    Frame(index, values)

  def convert(frame: Frame): String =
    s"frame ${frame.index} " + frame.values
      .map(_.mkString(" "))
      .mkString(start = "{\n\t", sep = "\n\t", end = "\n}")
}
