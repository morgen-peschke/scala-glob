package com.peschke.scalaglob.core

import com.typesafe.scalalogging.LazyLogging
import scala.util.matching.Regex

case class Glob(chunks: Glob.Chunk*) extends LazyLogging {
  assert(chunks.nonEmpty, "Glob must contain at least one chunk")

  /** Return a string which, when passed through [[Glob.apply]],
    * produces an equivalent [[Glob]]
    *
    * @return the glob as a string
    */
  def describe: String = chunks.map(_.describe).mkString

  /** Return true if the glob would expand to match a file with this name.
    *
    * That's the platonic ideal anyway, as implementations in the real
    * world vary, and [[Glob]] is still pretty raw.
    *
    * @return true if the input matches the glob
    */
  def test(input: String)(implicit settings: Glob.Settings): Boolean =
    input match {
      case regex() => true
      case _ =>
        if (settings.logFailures) {
          logger.warn(s"""[$describe] "$input" did not match regex: $regex""")
        }
        false
    }

  /** Return a [[Regex]] which matches the same input as this [[Glob]].
    *
    * The actual match in [[Glob.test]] uses this [[Regex]], so
    * exposing it here really helps when debugging.
    *
    * @return an equivalent [[Regex]]
    */
  val regex: Regex = chunks.map(_.regex).mkString.r
}

object Glob {
  /** Allows configuring various behaviors.
    *
    * Currently only toggles logging failures to parse and match.
    */
  case class Settings(logFailures: Boolean = false)

  implicit val defaultSettings: Settings = Settings()

  /** Parse the string to a [[util.Result]]
    *
    * @return [[util.Success]] if `source` is a valid glob, [[util.Failure]] otherwise
    */
  def apply(source: String): utils.Result = GlobParser.parse(source)

  /** [[Chunk]] subclasses provide or build [[String]] instances which
    * will be used to construct the final [[Regex]]
    */
  sealed trait Chunk {
    import Chunk._

    def describe: String = this match {
      case Literal(text) => text
      case AnyChar => "?"
      case AnyString => "*"
      case Bracket(body) => "[" + body + "]"
    }

    def regex: String = this match {
      case Literal(text) => Regex.quote(text)
      case AnyChar => "."
      case AnyString => ".*"
      case b @ Bracket(_) => "[" + b.quotedBody + "]"
    }
  }

  object Chunk {
    case class Literal(text: String) extends Chunk
    case object AnyChar extends Chunk
    case object AnyString extends Chunk
    case class Bracket(body: String) extends Chunk {
      private[Glob] def quotedBody = body.toList match {
        case '!' :: str => "^" + Bracket.quote(str.mkString)
        case _ => Bracket.quote(body)
      }
    }
    object Bracket {
      def quote(text: String): String = text.flatMap {
        case ']' => """\]"""
        case '[' => """\["""
        case c => c.toString
      }
    }
  }
}
