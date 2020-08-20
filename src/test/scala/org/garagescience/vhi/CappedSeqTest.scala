package org.garagescience.vhi

import scala.util.Random
import scala.collection.immutable.BitSet
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import scala.util.Either
import scala.util.{Failure, Success, Try}
import scala.util.control.Exception

import IntegralTypeOps._
import FixedSizeSequence._

object TestCappedSeq extends Properties("TortureTheFixedSeqCollection") {

  val seqSize = 20
  val smallSeqSize = 3
  val smallLong = Gen.choose(0L, 255L)
  val smallInt = Gen.choose(0, 255)

  object FixedSeq extends FixedSizeSeqFactory(capacity = 3)

  // this generatess a fixed-size list generator, with the
  // size of the list det'd by the maxSize parameter
  def genFixedSizeSeq[T](maxSize: Int, g: Gen[T]): Gen[Seq[T]] =
    Gen.listOfN(maxSize, g)

  // this generates a list generator of size [0 .. maxsize]
  def genBoundedList[T](maxSize: Int, g: Gen[T]): Gen[List[T]] = {
    Gen.choose(0, maxSize) flatMap { sz => Gen.listOfN(sz, g) }
  }


  // TODO: we're going to need a generator for FixedSizeSeq

  val fxs1 = FixedSeq(1, 2, 3)

  // okay, let's get these factored into a test ...


  property("checkLength") = {
    fxs1.length == 3
  }


  property("getARandomListAndDoSomethingWithIt") = {

    forAll(smallInt) { sequenceLength: Int =>
      val smallFixedSizedGen = genFixedSizeSeq[Int](sequenceLength, smallInt)
      val _tmp_l = smallFixedSizedGen.sample.get
      object _FixedSeq extends FixedSizeSeqFactory(capacity = sequenceLength)
      val _tmp_xfs = _FixedSeq(_tmp_l: _*)
      _tmp_xfs.length == sequenceLength
    }
  }

  property("createAnEmptyFixedLengthSequenceLotsOfTime") = {
    forAll(smallInt) { sequenceLength: Int =>
      // create an empty FixedSeq of the correct capacity ...
      object _FixedSeq extends FixedSizeSeqFactory(capacity = sequenceLength)
      val emptyFixedSeq = _FixedSeq.empty
      emptyFixedSeq.length == 0
    }
  }

  property("isItEmptyOrWhat") = {
    forAll(smallInt) { sequenceLength: Int =>
      // create an empty FixedSeq of the correct capacity ...
      object _FixedSeq extends FixedSizeSeqFactory(capacity = sequenceLength)
      val emptyFixedSeq = _FixedSeq.empty
      emptyFixedSeq.isEmpty == true
    }
  }


  property("appendAnotherRandomSequence") = {

    forAll(smallInt) { sequenceLength: Int =>
      val randomSeqLength = smallInt.sample.get

      object CreateFixedSeq1 extends
        FixedSizeSeqFactory(capacity=randomSeqLength)

      // using CreateFixedSeq1.empty means we get the correct type
      val appendableFS1 = CreateFixedSeq1.empty ++  {
        for { x <- 1 to randomSeqLength } yield smallInt.sample.get
      }

      appendableFS1.length == randomSeqLength
    }
  }

  // if we overfill the sequence, it discards elements from the head
  // so if we overfill the sequence by 1, the head == 2

  property("testOverFill") = {

    forAll(smallInt) { fixedSequenceLength: Int =>

      if (fixedSequenceLength == 0) {
        true
      } else {
        object CreateFixedSeq1 extends
          FixedSizeSeqFactory(capacity = fixedSequenceLength)

        val _3AppendableSeq = CreateFixedSeq1.empty ++ {
          for {x <- 1 to fixedSequenceLength + 1} yield x
        }

        _3AppendableSeq.head == 2
      }

    }
  }



  /*
  property("createAnEmptyFixedSeqAndThenAddARandomSeqToIt") = {

  }
  */

  // val fixedSeq = FixedSeq(1, 2, 3, 4, 5)

  /*
  val vectors: Gen[Vector] =
  for {
    x <- Gen.choose(0, 255)
    y <- Gen.choose(0, 255)
    z <- Gen.choose(0, 255)
  } yield Vector(x, y, z)
   */

}