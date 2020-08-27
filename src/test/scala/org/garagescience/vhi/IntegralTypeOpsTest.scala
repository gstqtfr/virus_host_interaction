package org.garagescience.vhi

import scala.collection.immutable.BitSet
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import scala.util.Either
import scala.util.{Try, Success, Failure}
import scala.util.control.Exception

import IntegralTypeOps.{toBitSet, wrapLong, wrapInt}

object ConvertIntegrals extends Properties("ConvertIntegrals") {

  // maybe we need some larger Longs & Ints here?
  val smallLong = Gen.choose(0L, 255L)
  val smallInt = Gen.choose(0, 255)
  val negativeLong = Gen.choose(-1000L, -1L)

  def convertLongToIntegral(l: Long): Boolean = {
    Try(toBitSet(l)) match {
      case Success(_) => true
      case Failure(e) => false
    }
  }

  def convertIntegerToIntegral(i: Int): Boolean = {
    Try(toBitSet(i)) match {
      case Success(_) => true
      case Failure(e) => false
    }
  }

  // we're throwing this a bunch of negative Longs, so it
  // *should* throw an execption
  def checkExceptionCausedByNegativeNumber(l: Long): Boolean = {
    Try(toBitSet(l)) match {
      case Failure(e) => true
      case Success(_) => false
    }
  }

  property("turnTinyLittleLongsIntoBitSets") =
    forAll(smallLong) { input: Long =>
      convertLongToIntegral(input)
    }


  property("turnTinyLittleIntsIntoBitSets") =
    forAll(smallInt) { input: Int =>
      convertIntegerToIntegral(input)
    }


  property("checkNegaitveValuesThrowAWobbler") =
    forAll(negativeLong) { input: Long =>
      checkExceptionCausedByNegativeNumber(input)
    }
}