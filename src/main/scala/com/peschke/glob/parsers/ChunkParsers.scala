package com.peschke.glob.parsers

import fastparse.noApi._

import com.peschke.glob.Glob.Chunk

object ChunkParsers {
  import DoNotIgnoreSpaces._

  def buildParserFromChunks(chunks: Seq[Chunk]): Parser[Unit] =
    P(chunks.map(_.parser).reduce((a, b) => P(a ~ b)) ~ End)

  def literalParser(string: String): Parser[Unit] = P(string)

  // Parsers are expensive to instantiate, so when possible we only
  // do this once.
  val anyCharParser: Parser[Unit] = AnyChar
}
