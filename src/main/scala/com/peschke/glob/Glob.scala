package com.peschke.glob

import com.typesafe.scalalogging.LazyLogging
import fastparse.noApi._

import com.peschke.glob.parsers.{ ChunkParsers, GlobParser }

case class Glob(parts: Glob.Chunk*) extends LazyLogging {
  assert(parts.nonEmpty, "Glob must contain at least one chunk")

  def describe: String = parts.map(_.describe).mkString

  def test(input: String)(implicit settings: Glob.Settings): Boolean =
    parser.parse(input) match {
      case Parsed.Success(_, _) => true
      case f @ Parsed.Failure(_, _, _) =>
        if (settings.logFailures) {
          logger.warn(s"""[$describe] "$input" did not match pattern: ${f.msg}""")
        }
        false
    }

  val parser: Parser[Unit] = ChunkParsers.buildParserFromChunks(parts)
}

object Glob {
  case class Settings(logFailures: Boolean = false)
  implicit val defaultSettings: Settings = Settings()

  sealed trait Chunk {
    import Chunk._

    def describe: String = this match {
      case Literal(text) => text
      case AnyChar => "?"
    }

    def parser: Parser[Unit]
  }

  object Chunk {
    case class Literal(text: String) extends Chunk {
      val parser: Parser[Unit] = ChunkParsers.literalParser(text)
    }

    case object AnyChar extends Chunk {
      def parser: Parser[Unit] = ChunkParsers.anyCharParser
    }
  }

  def apply(source: String): GlobParser.Result = GlobParser.parse(source)
}
