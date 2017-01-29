package com.peschke.glob

import org.scalacheck.Gen
import org.scalatest.WordSpec
import org.scalatest.Matchers

import com.peschke.glob.parsers.GlobParser.{ Failure, Success }
import Glob.Chunk._

class GlobSpec extends WordSpec with Matchers {
  import GlobSpec._

  "Glob.describe" should {
    "print an equivalent glob" in {
      TestGlobs.LiteralString.describe shouldBe "literal string"
      TestGlobs.LiteralStringCaseCheck.describe shouldBe "Literal String"

      TestGlobs.SingleCharWildCard.describe shouldBe "?"
      TestGlobs.ThreeCharWildCards.describe shouldBe "???"

      TestGlobs.LiteralBeforeCharWildCard.describe shouldBe "literal?"
      TestGlobs.LiteralAfterCharWildCard.describe shouldBe "?literal"
      TestGlobs.LiteralsSurroundCharWildCard.describe shouldBe "literal?string"
    }
  }

  "Glob.test(input)" should {
    "return true if input matches the glob" in {
      implicit val settings = Glob.Settings(logFailures = true)

      TestGlobs.LiteralString.test("literal string") shouldBe true
      TestGlobs.LiteralStringCaseCheck.test("Literal String") shouldBe true

      TestGlobs.SingleCharWildCard.test("a") shouldBe true
      TestGlobs.SingleCharWildCard.test("A") shouldBe true
      TestGlobs.SingleCharWildCard.test(" ") shouldBe true
      TestGlobs.SingleCharWildCard.test("?") shouldBe true

      TestGlobs.ThreeCharWildCards.test("aaa") shouldBe true
      TestGlobs.ThreeCharWildCards.test("ABC") shouldBe true
      TestGlobs.ThreeCharWildCards.test(" . ") shouldBe true
      TestGlobs.ThreeCharWildCards.test("???") shouldBe true

      TestGlobs.LiteralBeforeCharWildCard.test("literal ") shouldBe true
      TestGlobs.LiteralBeforeCharWildCard.test("literal-") shouldBe true
      TestGlobs.LiteralBeforeCharWildCard.test("literal_") shouldBe true
      TestGlobs.LiteralBeforeCharWildCard.test("literalU") shouldBe true

      TestGlobs.LiteralAfterCharWildCard.test(" literal") shouldBe true
      TestGlobs.LiteralAfterCharWildCard.test("-literal") shouldBe true
      TestGlobs.LiteralAfterCharWildCard.test("_literal") shouldBe true
      TestGlobs.LiteralAfterCharWildCard.test("Uliteral") shouldBe true

      TestGlobs.LiteralsSurroundCharWildCard.test("literal string") shouldBe true
      TestGlobs.LiteralsSurroundCharWildCard.test("literal-string") shouldBe true
      TestGlobs.LiteralsSurroundCharWildCard.test("literal_string") shouldBe true
      TestGlobs.LiteralsSurroundCharWildCard.test("literalUstring") shouldBe true
    }

    "return false if input does not match the glob" in {
      Glob(Literal("test"), AnyChar)

      TestGlobs.LiteralString.test("") shouldBe false
      TestGlobs.LiteralString.test("not the literal string") shouldBe false
      TestGlobs.LiteralStringCaseCheck.test("literal string") shouldBe false

      TestGlobs.SingleCharWildCard.test("") shouldBe false
      TestGlobs.SingleCharWildCard.test("aa") shouldBe false

      TestGlobs.ThreeCharWildCards.test("a") shouldBe false
      TestGlobs.ThreeCharWildCards.test("ab") shouldBe false
      TestGlobs.ThreeCharWildCards.test("abcd") shouldBe false

      TestGlobs.LiteralBeforeCharWildCard.test("literal") shouldBe false
      TestGlobs.LiteralBeforeCharWildCard.test("wrong") shouldBe false
      TestGlobs.LiteralBeforeCharWildCard.test("literal..") shouldBe false

      TestGlobs.LiteralAfterCharWildCard.test("literal") shouldBe false
      TestGlobs.LiteralAfterCharWildCard.test(".wrong") shouldBe false
      TestGlobs.LiteralAfterCharWildCard.test(" wrong") shouldBe false

      TestGlobs.LiteralsSurroundCharWildCard.test("literal ") shouldBe false
      TestGlobs.LiteralsSurroundCharWildCard.test("literal-") shouldBe false
      TestGlobs.LiteralsSurroundCharWildCard.test("literal.wrong") shouldBe false
    }
  }

  "Glob.apply(source)" should afterWord("parse the source to"){
    "a Success(Glob) if the syntax is correct" in {
      Glob("literal string") shouldBe Success(TestGlobs.LiteralString)
      Glob("Literal String") shouldBe Success(TestGlobs.LiteralStringCaseCheck)

      Glob("?") shouldBe Success(TestGlobs.SingleCharWildCard)
      Glob("???") shouldBe Success(TestGlobs.ThreeCharWildCards)

      Glob("literal?") shouldBe Success(TestGlobs.LiteralBeforeCharWildCard)
      Glob("?literal") shouldBe Success(TestGlobs.LiteralAfterCharWildCard)
      Glob("literal?string") shouldBe Success(TestGlobs.LiteralsSurroundCharWildCard)
    }

    "a Failure if the syntax is not correct" in {
      Glob("") shouldBe Failure("glob string was empty")
    }
  }
}

object GlobSpec {
  object TestGlobs {
    val LiteralString = Glob(Literal("literal string"))
    val LiteralStringCaseCheck = Glob(Literal("Literal String"))

    val SingleCharWildCard = Glob(AnyChar)
    val ThreeCharWildCards = Glob(AnyChar, AnyChar, AnyChar)

    val LiteralBeforeCharWildCard = Glob(Literal("literal"), AnyChar)
    val LiteralAfterCharWildCard = Glob(AnyChar, Literal("literal"))
    val LiteralsSurroundCharWildCard =
      Glob(
        Literal("literal"),
        AnyChar,
        Literal("string"))
  }
}
