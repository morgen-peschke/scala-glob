package com.peschke.glob

import org.scalacheck.Gen
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.matchers.{ Matcher, MatchResult }

import com.peschke.glob.parsers.GlobParser.{ Failure, Success }
import Glob.Chunk._

class GlobSpec extends WordSpec with Matchers {
  import GlobSpec._

  def matchInput(input: String): Matcher[Glob] = Matcher { glob =>
    MatchResult(
      glob.test(input),
      s"""${glob.describe} did not match "$input" using regex "${glob.regex}"""",
      s"""${glob.describe} matched "$input" using regex "${glob.regex}""""
    )
  }

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

      TestGlobs.LiteralString should matchInput("literal string")
      TestGlobs.LiteralString should matchInput("literal string")
      TestGlobs.LiteralStringCaseCheck should matchInput("Literal String")
      TestGlobs.LiteralStringQuoteCheck should matchInput(".")

      TestGlobs.SingleCharWildCard should matchInput("a")
      TestGlobs.SingleCharWildCard should matchInput("A")
      TestGlobs.SingleCharWildCard should matchInput(" ")
      TestGlobs.SingleCharWildCard should matchInput("?")

      TestGlobs.ThreeCharWildCards should matchInput("aaa")
      TestGlobs.ThreeCharWildCards should matchInput("ABC")
      TestGlobs.ThreeCharWildCards should matchInput(" . ")

      TestGlobs.LiteralBeforeCharWildCard should matchInput("literal-")
      TestGlobs.LiteralBeforeCharWildCard should matchInput("literal ")
      TestGlobs.LiteralBeforeCharWildCard should matchInput("literalU")

      TestGlobs.LiteralAfterCharWildCard should matchInput("-literal")
      TestGlobs.LiteralAfterCharWildCard should matchInput(" literal")
      TestGlobs.LiteralAfterCharWildCard should matchInput("Uliteral")

      TestGlobs.LiteralsSurroundCharWildCard should matchInput("literal-string")
      TestGlobs.LiteralsSurroundCharWildCard should matchInput("literal string")
      TestGlobs.LiteralsSurroundCharWildCard should matchInput("literalUstring")

      TestGlobs.SingleStringWildCard should matchInput("")
      TestGlobs.SingleStringWildCard should matchInput("a")
      TestGlobs.SingleStringWildCard should matchInput("abc")
      TestGlobs.SingleStringWildCard should matchInput(" a")
      TestGlobs.SingleStringWildCard should matchInput(" abc")
      TestGlobs.SingleStringWildCard should matchInput("-abc")

      TestGlobs.LiteralBeforeStringWildCard should matchInput("literal")
      TestGlobs.LiteralBeforeStringWildCard should matchInput("literal-")
      TestGlobs.LiteralBeforeStringWildCard should matchInput("literal ")
      TestGlobs.LiteralBeforeStringWildCard should matchInput("literalU")
      TestGlobs.LiteralBeforeStringWildCard should matchInput("literal a")
      TestGlobs.LiteralBeforeStringWildCard should matchInput("literal-a")

      TestGlobs.LiteralAfterStringWildCard should matchInput("literal")
      TestGlobs.LiteralAfterStringWildCard should matchInput("-literal")
      TestGlobs.LiteralAfterStringWildCard should matchInput(" literal")
      TestGlobs.LiteralAfterStringWildCard should matchInput("Uliteral")
      TestGlobs.LiteralAfterStringWildCard should matchInput("  literal")
      TestGlobs.LiteralAfterStringWildCard should matchInput("a-literal")

      TestGlobs.LiteralsSurroundStringWildCard should matchInput("literal-string")
      TestGlobs.LiteralsSurroundStringWildCard should matchInput("literal string")
      TestGlobs.LiteralsSurroundStringWildCard should matchInput("literalUstring")
      TestGlobs.LiteralsSurroundStringWildCard should matchInput("literalstring")
      TestGlobs.LiteralsSurroundStringWildCard should matchInput("literal-test-string")
      TestGlobs.LiteralsSurroundStringWildCard should matchInput("literal test string")

      TestGlobs.SingleSimpleBracket should matchInput("a")
      TestGlobs.SingleSimpleBracket should matchInput("B")
      TestGlobs.SingleSimpleBracket should matchInput("c")

      TestGlobs.BracketContainingBracketChars should matchInput("[")
      TestGlobs.BracketContainingBracketChars should matchInput("]")

      TestGlobs.LiteralsSurroundBracket should matchInput("literal_string")
      TestGlobs.LiteralsSurroundBracket should matchInput("literal.string")
      TestGlobs.LiteralsSurroundBracket should matchInput("literal-string")

      TestGlobs.BracketWithRange should matchInput("a")
      TestGlobs.BracketWithRange should matchInput("b")
      TestGlobs.BracketWithRange should matchInput(" ")
      TestGlobs.BracketWithRange should matchInput("Y")
      TestGlobs.BracketWithRange should matchInput("Z")

      TestGlobs.BracketStartsWithDash should matchInput("-")
      TestGlobs.BracketStartsWithDash should matchInput("e")
      TestGlobs.BracketEndsWithDash should matchInput("-")
      TestGlobs.BracketEndsWithDash should matchInput("e")

      TestGlobs.ComplementedBracket should matchInput("A")
      TestGlobs.ComplementedBracket should matchInput("b")
      TestGlobs.ComplementedBracket should matchInput("d")

      TestGlobs.BracketWithExclamationPoint should matchInput("a")
      TestGlobs.BracketWithExclamationPoint should matchInput("!")
      TestGlobs.BracketWithExclamationPoint should matchInput("b")
    }

    "return false if input does not match the glob" in {
      TestGlobs.LiteralString should not(matchInput(""))
      TestGlobs.LiteralString should not(matchInput("not the literal string"))
      TestGlobs.LiteralStringCaseCheck should not(matchInput("literal string"))
      TestGlobs.LiteralStringQuoteCheck should not(matchInput("a"))
      TestGlobs.LiteralStringQuoteCheck should not(matchInput(" "))

      TestGlobs.SingleCharWildCard should not(matchInput(""))
      TestGlobs.SingleCharWildCard should not(matchInput("aa"))

      TestGlobs.ThreeCharWildCards should not(matchInput("a"))
      TestGlobs.ThreeCharWildCards should not(matchInput("ab"))
      TestGlobs.ThreeCharWildCards should not(matchInput("abcd"))

      TestGlobs.LiteralBeforeCharWildCard should not(matchInput("literal"))
      TestGlobs.LiteralBeforeCharWildCard should not(matchInput("wrong"))
      TestGlobs.LiteralBeforeCharWildCard should not(matchInput("literal  "))

      TestGlobs.LiteralAfterCharWildCard should not(matchInput("literal"))
      TestGlobs.LiteralAfterCharWildCard should not(matchInput(".wrong"))
      TestGlobs.LiteralAfterCharWildCard should not(matchInput(" wrong"))

      TestGlobs.LiteralsSurroundCharWildCard should not(matchInput("literal-"))
      TestGlobs.LiteralsSurroundCharWildCard should not(matchInput("literal "))
      TestGlobs.LiteralsSurroundCharWildCard should not(matchInput("literal.wrong"))

      TestGlobs.LiteralBeforeStringWildCard should not(matchInput("wrong"))

      TestGlobs.LiteralAfterStringWildCard should not(matchInput(".wrong"))
      TestGlobs.LiteralAfterStringWildCard should not(matchInput(" wrong"))

      TestGlobs.LiteralsSurroundStringWildCard should not(matchInput("literal-"))
      TestGlobs.LiteralsSurroundStringWildCard should not(matchInput("literal "))
      TestGlobs.LiteralsSurroundStringWildCard should not(matchInput("literal.wrong"))

      TestGlobs.SingleSimpleBracket should not(matchInput("A"))
      TestGlobs.SingleSimpleBracket should not(matchInput("b"))
      TestGlobs.SingleSimpleBracket should not(matchInput(" "))

      TestGlobs.BracketContainingBracketChars should not(matchInput(""))

      TestGlobs.LiteralsSurroundBracket should not(matchInput("wrong_string"))
      TestGlobs.LiteralsSurroundBracket should not(matchInput("literal.wrong"))
      TestGlobs.LiteralsSurroundBracket should not(matchInput("literal string"))

      TestGlobs.BracketWithRange should not(matchInput("A"))
      TestGlobs.BracketWithRange should not(matchInput("c"))
      TestGlobs.BracketWithRange should not(matchInput("."))
      TestGlobs.BracketWithRange should not(matchInput("X"))
      TestGlobs.BracketWithRange should not(matchInput("z"))

      TestGlobs.BracketStartsWithDash should not(matchInput("a"))
      TestGlobs.BracketStartsWithDash should not(matchInput("E"))
      TestGlobs.BracketEndsWithDash should not(matchInput("a"))
      TestGlobs.BracketEndsWithDash should not(matchInput("E"))

      TestGlobs.ComplementedBracket should not(matchInput("a"))
      TestGlobs.ComplementedBracket should not(matchInput("B"))
      TestGlobs.ComplementedBracket should not(matchInput("c"))

      TestGlobs.BracketWithExclamationPoint should not(matchInput("A"))
      TestGlobs.BracketWithExclamationPoint should not(matchInput(" "))
      TestGlobs.BracketWithExclamationPoint should not(matchInput("C"))
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
