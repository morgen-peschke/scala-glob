package com.peschke.scalaglob

/** This provides aliases to reduce the import tax
  */
package object api {
  // Note to self: the var is needed so we can chain into the
  // companion object for the type alias
  type Glob = core.Glob
  val Glob = core.Glob

  type Success = core.utils.Success
  val Success = core.utils.Success

  type Failure = core.utils.Failure
  val Failure = core.utils.Failure
}
