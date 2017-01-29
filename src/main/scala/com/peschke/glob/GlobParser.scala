package com.peschke.glob

import Glob.Chunk._

object GlobParser {
  sealed trait Result
  case class Success(glob: Glob) extends Result
  case class Failure(error: String) extends Result

  def parse(source: String): Result =
    if (source.isEmpty) Failure("glob string was empty")
    else Success(Glob(Literal(source)))
}
