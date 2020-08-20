package org.garagescience.vhi

import scala.collection.immutable.BitSet
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}

import scala.util.Either

import org.garagescience.vhi.BitSetOps._

// so this is going to be a general property test
// the inspiration comes from:

// https://alvinalexander.com/scala/fp-book/scalacheck-1-introduction/

// we luv Alvin

object TurnStuffIntoBitSets extends Properties("TurnStuffIntoBitSets") {

  val smallLong = Gen.choose(0L, 255L)

  def checkImplicitLongConversion(input: Long): Boolean = {
    val bits1 = input.toBitSet
    val s = bits1.toBinaryString
    // this is an Option, so we could do some pattern matching, but
    // this'll do for now ...
    val l = s.toLongFromBinaryString.get
    l == input
  }

  property("turnLongIntoBitsAndBack") =
    forAll(smallLong) { input: Long =>
      checkImplicitLongConversion(input)
    }

}

