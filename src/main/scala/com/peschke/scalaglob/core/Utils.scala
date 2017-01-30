package com.peschke.scalaglob.core

object utils {
  /** Wraps success and failure while parsing a Glob from String
    */
  sealed trait Result
  case class Success(glob: Glob) extends Result
  case class Failure(error: String) extends Result

  /** FastParse error messages are much nicer when
    * `CharsWhile(<function1>)` can be replaced with something
    * meaningful like `CharsWhile(expecting end bracket)`.
    */
  case class Predicate[T](name: String)(f: T => Boolean) extends (T => Boolean){
    def apply(t: T) = f(t)
    override def toString() = name
  }
}
