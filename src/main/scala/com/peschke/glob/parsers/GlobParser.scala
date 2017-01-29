package com.peschke.glob.parsers

import com.typesafe.scalalogging.LazyLogging
import fastparse.noApi._

import com.peschke.glob.Glob
import Glob.Chunk

object GlobParser extends LazyLogging {
  import DoNotIgnoreSpaces._

  sealed trait Result
  case class Success(glob: Glob) extends Result
  case class Failure(error: String) extends Result

  def parse(source: String)(implicit settings: Glob.Settings): Result =
    if (source.isEmpty) Failure("glob string was empty")
    else globParser.parse(source) match {
      case Parsed.Success(glob, _) => Success(glob)
      case f @ Parsed.Failure(_, _, _) =>
        if (settings.logFailures) {
          logger.warn(s"""Unable to parse "$source" to glob: ${f.msg}""")
        }
        Failure(f.msg)
    }

  private val charWildCardParser: Parser[Chunk] =
    P("?")
      .opaque("?")
      .map(_ => Chunk.AnyChar)

  private val literalParser: Parser[Chunk] =
    P(CharsWhile(_ != '?').!)
      .opaque("<literal>")
      .map(Chunk.Literal(_))

  private val globParser: Parser[Glob] =
    P((charWildCardParser | literalParser).rep ~ End)
      .map(Glob(_:_*))
}
