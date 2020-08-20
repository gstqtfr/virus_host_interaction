package org.garagescience.vhi

import scala.collection.immutable.BitSet
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}

import scala.util.Either
import scala.util.{Failure, Success, Try}
import scala.util.control.Exception

object BuildFixedSeqInRange extends Properties("BuildFixedSeqInRange") {

  val smallLong = Gen.choose(0L, 255L)
  val smallInt = Gen.choose(0, 255)



}