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
      TestGlobs.LiteralStringQuoteCheck.describe shouldBe "."

      TestGlobs.SingleCharWildCard.describe shouldBe "?"
      TestGlobs.ThreeCharWildCards.describe shouldBe "???"

      TestGlobs.LiteralBeforeCharWildCard.describe shouldBe "literal?"
      TestGlobs.LiteralAfterCharWildCard.describe shouldBe "?literal"
      TestGlobs.LiteralsSurroundCharWildCard.describe shouldBe "literal?string"

      TestGlobs.SingleStringWildCard.describe shouldBe "*"

      TestGlobs.LiteralBeforeStringWildCard.describe shouldBe "literal*"
      TestGlobs.LiteralAfterStringWildCard.describe shouldBe "*literal"
      TestGlobs.LiteralsSurroundStringWildCard.describe shouldBe "literal*string"

      TestGlobs.SingleSimpleBracket.describe shouldBe "[aBc]"
      TestGlobs.BracketContainingBracketChars.describe shouldBe "[][]"
      TestGlobs.LiteralsSurroundBracket.describe shouldBe "literal[_.-]string"

      TestGlobs.BracketWithRange.describe shouldBe "[a-b Y-Z]"
      TestGlobs.BracketStartsWithDash.describe shouldBe "[-e]"
      TestGlobs.BracketEndsWithDash.describe shouldBe "[e-]"

      TestGlobs.ComplementedBracket.describe shouldBe "[!aBc]"
      TestGlobs.BracketWithExclamationPoint.describe shouldBe "[a!b]"
    }
  }

  "Glob.test(input)" should {
    "return true if input matches the glob" in {
      implicit val settings = Glob.Settings(logFailures = true)

      TestGlobs.LiteralString.test("literal string") shouldBe true
      TestGlobs.LiteralStringCaseCheck.test("Literal String") shouldBe true
      TestGlobs.LiteralStringQuoteCheck.test(".") shouldBe true

      TestGlobs.SingleCharWildCard.test("a") shouldBe true
      TestGlobs.SingleCharWildCard.test("A") shouldBe true
      TestGlobs.SingleCharWildCard.test(" ") shouldBe true
      TestGlobs.SingleCharWildCard.test("?") shouldBe true

      TestGlobs.ThreeCharWildCards.test("aaa") shouldBe true
      TestGlobs.ThreeCharWildCards.test("ABC") shouldBe true
      TestGlobs.ThreeCharWildCards.test(" . ") shouldBe true

      TestGlobs.LiteralBeforeCharWildCard.test("literal-") shouldBe true
      TestGlobs.LiteralBeforeCharWildCard.test("literal ") shouldBe true
      TestGlobs.LiteralBeforeCharWildCard.test("literalU") shouldBe true

      TestGlobs.LiteralAfterCharWildCard.test("-literal") shouldBe true
      TestGlobs.LiteralAfterCharWildCard.test(" literal") shouldBe true
      TestGlobs.LiteralAfterCharWildCard.test("Uliteral") shouldBe true

      TestGlobs.LiteralsSurroundCharWildCard.test("literal-string") shouldBe true
      TestGlobs.LiteralsSurroundCharWildCard.test("literal string") shouldBe true
      TestGlobs.LiteralsSurroundCharWildCard.test("literalUstring") shouldBe true

      TestGlobs.SingleStringWildCard.test("") shouldBe true
      TestGlobs.SingleStringWildCard.test("a") shouldBe true
      TestGlobs.SingleStringWildCard.test("abc") shouldBe true
      TestGlobs.SingleStringWildCard.test(" a") shouldBe true
      TestGlobs.SingleStringWildCard.test(" abc") shouldBe true
      TestGlobs.SingleStringWildCard.test("-abc") shouldBe true

      TestGlobs.LiteralBeforeStringWildCard.test("literal") shouldBe true
      TestGlobs.LiteralBeforeStringWildCard.test("literal-") shouldBe true
      TestGlobs.LiteralBeforeStringWildCard.test("literal ") shouldBe true
      TestGlobs.LiteralBeforeStringWildCard.test("literalU") shouldBe true
      TestGlobs.LiteralBeforeStringWildCard.test("literal a") shouldBe true
      TestGlobs.LiteralBeforeStringWildCard.test("literal-a") shouldBe true

      TestGlobs.LiteralAfterStringWildCard.test("literal") shouldBe true
      TestGlobs.LiteralAfterStringWildCard.test("-literal") shouldBe true
      TestGlobs.LiteralAfterStringWildCard.test(" literal") shouldBe true
      TestGlobs.LiteralAfterStringWildCard.test("Uliteral") shouldBe true
      TestGlobs.LiteralAfterStringWildCard.test("  literal") shouldBe true
      TestGlobs.LiteralAfterStringWildCard.test("a-literal") shouldBe true

      TestGlobs.LiteralsSurroundStringWildCard.test("literal-string") shouldBe true
      TestGlobs.LiteralsSurroundStringWildCard.test("literal string") shouldBe true
      TestGlobs.LiteralsSurroundStringWildCard.test("literalUstring") shouldBe true
      TestGlobs.LiteralsSurroundStringWildCard.test("literalstring") shouldBe true
      TestGlobs.LiteralsSurroundStringWildCard.test("literal-test-string") shouldBe true
      TestGlobs.LiteralsSurroundStringWildCard.test("literal test string") shouldBe true

      TestGlobs.SingleSimpleBracket.test("a") shouldBe true
      TestGlobs.SingleSimpleBracket.test("B") shouldBe true
      TestGlobs.SingleSimpleBracket.test("c") shouldBe true

      TestGlobs.BracketContainingBracketChars.test("[") shouldBe true
      TestGlobs.BracketContainingBracketChars.test("]") shouldBe true

      TestGlobs.LiteralsSurroundBracket.test("literal_string") shouldBe true
      TestGlobs.LiteralsSurroundBracket.test("literal.string") shouldBe true
      TestGlobs.LiteralsSurroundBracket.test("literal-string") shouldBe true

      TestGlobs.BracketWithRange.test("a") shouldBe true
      TestGlobs.BracketWithRange.test("b") shouldBe true
      TestGlobs.BracketWithRange.test(" ") shouldBe true
      TestGlobs.BracketWithRange.test("Y") shouldBe true
      TestGlobs.BracketWithRange.test("Z") shouldBe true

      TestGlobs.BracketStartsWithDash.test("-") shouldBe true
      TestGlobs.BracketStartsWithDash.test("e") shouldBe true
      TestGlobs.BracketEndsWithDash.test("-") shouldBe true
      TestGlobs.BracketEndsWithDash.test("e") shouldBe true

      TestGlobs.ComplementedBracket.test("A") shouldBe true
      TestGlobs.ComplementedBracket.test("b") shouldBe true
      TestGlobs.ComplementedBracket.test("d") shouldBe true

      TestGlobs.BracketWithExclamationPoint.test("a") shouldBe true
      TestGlobs.BracketWithExclamationPoint.test("!") shouldBe true
      TestGlobs.BracketWithExclamationPoint.test("b") shouldBe true
    }

    "return false if input does not match the glob" in {
      Glob(Literal("test"), AnyChar)

      TestGlobs.LiteralString.test("") shouldBe false
      TestGlobs.LiteralString.test("not the literal string") shouldBe false
      TestGlobs.LiteralStringCaseCheck.test("literal string") shouldBe false
      TestGlobs.LiteralStringQuoteCheck.test("a") shouldBe false
      TestGlobs.LiteralStringQuoteCheck.test(" ") shouldBe false

      TestGlobs.SingleCharWildCard.test("") shouldBe false
      TestGlobs.SingleCharWildCard.test("aa") shouldBe false

      TestGlobs.ThreeCharWildCards.test("a") shouldBe false
      TestGlobs.ThreeCharWildCards.test("ab") shouldBe false
      TestGlobs.ThreeCharWildCards.test("abcd") shouldBe false

      TestGlobs.LiteralBeforeCharWildCard.test("literal") shouldBe false
      TestGlobs.LiteralBeforeCharWildCard.test("wrong") shouldBe false
      TestGlobs.LiteralBeforeCharWildCard.test("literal  ") shouldBe false

      TestGlobs.LiteralAfterCharWildCard.test("literal") shouldBe false
      TestGlobs.LiteralAfterCharWildCard.test(".wrong") shouldBe false
      TestGlobs.LiteralAfterCharWildCard.test(" wrong") shouldBe false

      TestGlobs.LiteralsSurroundCharWildCard.test("literal-") shouldBe false
      TestGlobs.LiteralsSurroundCharWildCard.test("literal ") shouldBe false
      TestGlobs.LiteralsSurroundCharWildCard.test("literal.wrong") shouldBe false

      TestGlobs.LiteralBeforeStringWildCard.test("wrong") shouldBe false

      TestGlobs.LiteralAfterStringWildCard.test(".wrong") shouldBe false
      TestGlobs.LiteralAfterStringWildCard.test(" wrong") shouldBe false

      TestGlobs.LiteralsSurroundStringWildCard.test("literal-") shouldBe false
      TestGlobs.LiteralsSurroundStringWildCard.test("literal ") shouldBe false
      TestGlobs.LiteralsSurroundStringWildCard.test("literal.wrong") shouldBe false

      TestGlobs.SingleSimpleBracket.test("A") shouldBe false
      TestGlobs.SingleSimpleBracket.test("b") shouldBe false
      TestGlobs.SingleSimpleBracket.test(" ") shouldBe false

      TestGlobs.BracketContainingBracketChars.test("") shouldBe false

      TestGlobs.LiteralsSurroundBracket.test("wrong_string") shouldBe false
      TestGlobs.LiteralsSurroundBracket.test("literal.wrong") shouldBe false
      TestGlobs.LiteralsSurroundBracket.test("literal string") shouldBe false

      TestGlobs.BracketWithRange.test("A") shouldBe false
      TestGlobs.BracketWithRange.test("c") shouldBe false
      TestGlobs.BracketWithRange.test(".") shouldBe false
      TestGlobs.BracketWithRange.test("X") shouldBe false
      TestGlobs.BracketWithRange.test("z") shouldBe false

      TestGlobs.BracketStartsWithDash.test("a") shouldBe false
      TestGlobs.BracketStartsWithDash.test("E") shouldBe false
      TestGlobs.BracketEndsWithDash.test("a") shouldBe false
      TestGlobs.BracketEndsWithDash.test("E") shouldBe false

      TestGlobs.ComplementedBracket.test("a") shouldBe false
      TestGlobs.ComplementedBracket.test("B") shouldBe false
      TestGlobs.ComplementedBracket.test("c") shouldBe false

      TestGlobs.BracketWithExclamationPoint.test("A") shouldBe false
      TestGlobs.BracketWithExclamationPoint.test(" ") shouldBe false
      TestGlobs.BracketWithExclamationPoint.test("C") shouldBe false
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

      Glob("*") shouldBe Success(TestGlobs.SingleStringWildCard)
      Glob("**") shouldBe Success(TestGlobs.SingleStringWildCard)

      Glob("literal*") shouldBe Success(TestGlobs.LiteralBeforeStringWildCard)
      Glob("*literal") shouldBe Success(TestGlobs.LiteralAfterStringWildCard)
      Glob("literal*string") shouldBe Success(TestGlobs.LiteralsSurroundStringWildCard)

      Glob("[aBc]") shouldBe Success(TestGlobs.SingleSimpleBracket)
      Glob("[][]") shouldBe Success(TestGlobs.BracketContainingBracketChars)
      Glob("literal[_.-]string") shouldBe Success(TestGlobs.LiteralsSurroundBracket)
      Glob("]string") shouldBe Success(Glob(Literal("]string")))

      Glob("[a-b Y-Z]") shouldBe Success(TestGlobs.BracketWithRange)
      Glob("[-e]") shouldBe Success(TestGlobs.BracketStartsWithDash)
      Glob("[e-]") shouldBe Success(TestGlobs.BracketEndsWithDash)

      Glob("[!aBc]") shouldBe Success(TestGlobs.ComplementedBracket)
      Glob("[a!b]") shouldBe Success(TestGlobs.BracketWithExclamationPoint)
    }

    "a Failure if the syntax is not correct" in {
      Glob("") shouldBe Failure("""(? | * | characterClass | literal):1:1 ...""""")
      Glob("[]") shouldBe Failure("""CharsWhile(expecting end bracket):1:3 ...""""")
      Glob("[") shouldBe Failure("""CharsWhile(expecting end bracket):1:2 ...""""")
      Glob("prefix[") shouldBe Failure("""CharsWhile(expecting end bracket):1:8 ...""""")
      Glob("[suffix") shouldBe Failure("""missing ']':1:8 ...""""")
    }
  }
}

object GlobSpec {
  object TestGlobs {
    val LiteralString = Glob(Literal("literal string"))
    val LiteralStringCaseCheck = Glob(Literal("Literal String"))
    val LiteralStringQuoteCheck = Glob(Literal("."))

    val SingleCharWildCard = Glob(AnyChar)
    val ThreeCharWildCards = Glob(AnyChar, AnyChar, AnyChar)

    val LiteralBeforeCharWildCard = Glob(Literal("literal"), AnyChar)
    val LiteralAfterCharWildCard = Glob(AnyChar, Literal("literal"))
    val LiteralsSurroundCharWildCard =
      Glob(
        Literal("literal"),
        AnyChar,
        Literal("string"))

    val SingleStringWildCard = Glob(AnyString)

    val LiteralBeforeStringWildCard = Glob(Literal("literal"), AnyString)
    val LiteralAfterStringWildCard = Glob(AnyString, Literal("literal"))
    val LiteralsSurroundStringWildCard =
      Glob(
        Literal("literal"),
        AnyString,
        Literal("string"))

    val SingleSimpleBracket = Glob(Bracket("aBc"))
    val BracketContainingBracketChars = Glob(Bracket("]["))
    val LiteralsSurroundBracket =
      Glob(
        Literal("literal"),
        Bracket("_.-"),
        Literal("string"))

    val BracketWithRange = Glob(Bracket("a-b Y-Z"))
    val BracketStartsWithDash = Glob(Bracket("-e"))
    val BracketEndsWithDash = Glob(Bracket("e-"))

    val ComplementedBracket = Glob(Bracket("!aBc"))
    val BracketWithExclamationPoint = Glob(Bracket("a!b"))
  }
}
