package com.peschke.glob

import fastparse.WhitespaceApi

package object parsers {
  /** We need to tell fastparse not to consume whitespace characters in
    * a couple of places, so this is as good a place as any.
    */
  val DoNotIgnoreSpaces = WhitespaceApi.Wrapper {
    fastparse.all.Pass
  }
}
