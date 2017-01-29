package com.peschke.glob

import fastparse.all._

case class Glob(parts: Glob.Chunk*) {
  assert(parts.nonEmpty, "Glob must contain at least one chunk")

  def describe: String = parts.map(_.describe).mkString
  def test(input: String): Boolean = parser.parse(input) match {
    case Parsed.Success(_, _) => true
    case _ => false
  }

  val parser: Parser[Unit] = parts.map(_.parser).reduce(_ ~ _)
}

object Glob {
  sealed trait Chunk {
    import Chunk._

    def describe: String = this match {
      case Literal(text) => text
    }

    def parser: Parser[Unit]
  }

  object Chunk {
    case class Literal(text: String) extends Chunk {
      val parser: Parser[Unit] = P(text)
    }
  }

  def apply(source: String): GlobParser.Result = GlobParser.parse(source)
}
