package com.peschke.scalaglob.core

import com.typesafe.scalalogging.LazyLogging
import fastparse.noApi._
import fastparse.WhitespaceApi

import Glob.Chunk
import utils.{ Failure, Result, Success, Predicate }

/** The primary method is [[GlobParser.parse]], which should mostly be
  * used by way of [[Glob.apply]]
  */
object GlobParser extends LazyLogging {
  val DoNotIgnoreSpaces = WhitespaceApi.Wrapper {
    // Tells FastParse not to discard whitespace
    fastparse.all.Pass
  }
  import DoNotIgnoreSpaces._

  /** Attempts to parse a [[String]] into a [[Glob]], wrapped in a [[Result]]
    *
    * If [[Glob.Settings.logFailures]] is `true` it will log failures
    * as warnings, with additional debug information.
    *
    * @return [[Success]] if `source` is a valid glob, [[Failure]] otherwise
    */
  def parse(source: String)(implicit settings: Glob.Settings): Result =
    globParser.parse(source) match {
      case Parsed.Success(glob, _) => Success(glob)
      case f @ Parsed.Failure(_, _, _) =>
        if (settings.logFailures) {
          logger.warn(s"""Unable to parse "$source" to glob: ${f.msg}""")
        }
        Failure(f.msg)
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
