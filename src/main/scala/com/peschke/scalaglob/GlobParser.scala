package com.peschke.scalaglob

import com.typesafe.scalalogging.LazyLogging
import fastparse.noApi._
import fastparse.WhitespaceApi

import Glob.Chunk

object GlobParser extends LazyLogging {
  val DoNotIgnoreSpaces = WhitespaceApi.Wrapper {
    // Tells FastParse not to discard whitespace
    fastparse.all.Pass
  }
  import DoNotIgnoreSpaces._

  sealed trait Result
  case class Success(glob: Glob) extends Result
  case class Failure(error: String) extends Result

  def parse(source: String)(implicit settings: Glob.Settings): Result =
    globParser.parse(source) match {
      case Parsed.Success(glob, _) => Success(glob)
      case f @ Parsed.Failure(_, _, _) =>
        if (settings.logFailures) {
          logger.warn(s"""Unable to parse "$source" to glob: ${f.msg}""")
        }
        Failure(f.msg)
    }

  case class Predicate[T](name: String)(f: T => Boolean) extends (T => Boolean){
    def apply(t: T) = f(t)
    override def toString() = name
  }

  private val charWildCard: Parser[Chunk] =
    P("?")
      .opaque("?")
      .map(_ => Chunk.AnyChar)

  private val stringWildCard: Parser[Chunk] =
    P("*".rep(1))
      .opaque("*")
      .map(_ => Chunk.AnyString)

  private val literal: Parser[Chunk] = {
    val isNotSpecialChar = Predicate("is not special character")(Set('?', '*', '[').andThen(!_))
    P(CharsWhile(isNotSpecialChar).!)
      .map(Chunk.Literal(_))
  }

  private val characterClass: Parser[Chunk] = {
    val expectingEndBracket = Predicate("expecting end bracket")((_: Char) != ']')
    P("[" ~/ "]".!.? ~ CharsWhile(expectingEndBracket).! ~ "]".opaque("missing ']'")).map {
      case (maybeBracket, chars) => Chunk.Bracket((maybeBracket.getOrElse("") + chars))
    }
  }

  private val endWithMessage = End.opaque("Unexpected end of input")

  private val chunkParser: Parser[Chunk] =
    P(charWildCard | stringWildCard | characterClass | literal)

  private val globParser: Parser[Glob] =
    P(chunkParser.rep(1) ~ endWithMessage)
      .map(Glob(_:_*))
}
