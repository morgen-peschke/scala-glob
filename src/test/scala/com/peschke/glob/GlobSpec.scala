package com.peschke.glob

import org.scalacheck.Gen
import org.scalatest.WordSpec
import org.scalatest.Matchers

import GlobParser.{ Failure, Success }
import Glob.Chunk._

class GlobSpec extends WordSpec with Matchers {
  "Glob.describe" should {
    "print an equivalent glob" in {
      Glob(Literal("test string")).describe shouldBe "test string"
    }
  }

  "Glob.test(input)" should {
    "return true if input matches the glob" in {
      Glob(Literal("test string")).test("test string") shouldBe true
    }

    "return false if input does not match the glob" in {
      Glob(Literal("test string")).test("") shouldBe false
      Glob(Literal("test string")).test("not the test string") shouldBe false
    }
  }

  "Glob.apply(source)" should afterWord("parse the source to"){
    "a Success(Glob) if the syntax is correct" in {
      Glob("test string") shouldBe Success(Glob(Literal("test string")))
    }

    "a Failure if the syntax is not correct" in {
      Glob("") shouldBe Failure("glob string was empty")
    }
  }
}
