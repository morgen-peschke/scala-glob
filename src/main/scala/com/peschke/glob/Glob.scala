package com.peschke.glob

import com.typesafe.scalalogging.LazyLogging
import scala.util.matching.Regex

import com.peschke.glob.parsers.{ ChunkParsers, GlobParser }

case class Glob(chunks: Glob.Chunk*) extends LazyLogging {
  assert(chunks.nonEmpty, "Glob must contain at least one chunk")

  def describe: String = chunks.map(_.describe).mkString

  def test(input: String)(implicit settings: Glob.Settings): Boolean =
    input match {
      case regex() => true
      case _ =>
        if (settings.logFailures) {
          logger.warn(s"""[$describe] "$input" did not match regex: $regex""")
        }
        false
    }

  val regex: Regex = chunks.map(_.regex).mkString.r
}

object Glob {
  case class Settings(logFailures: Boolean = false)

  implicit val defaultSettings: Settings = Settings()

  sealed trait Chunk {
    import Chunk._

    def describe: String = this match {
      case Literal(text) => text
      case AnyChar => "?"
      case AnyString => "*"
    }

    def regex: String = this match {
      case Literal(text) => Regex.quote(text)
      case AnyChar => "."
      case AnyString => ".*"
    }
  }

  object Chunk {
    case class Literal(text: String) extends Chunk
    case object AnyChar extends Chunk
    case object AnyString extends Chunk
  }

  def apply(source: String): GlobParser.Result = GlobParser.parse(source)
}
