package org.garagescience.vhi

import scala.collection.immutable.BitSet
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Gen.posNum
import scala.util.Either
import org.garagescience.vhi.AdaptiveTechniques._
import org.garagescience.vhi.BitSetOps._

object MutateTheHellOutOfIt extends Properties("MUtateTheHellOutOfAnInteger") {

  val length=64

  // what do we want? we want the output Int to be
  // not equal to the input Int

  def checkOneHotSpotMutation(input: Int): Boolean = {
    val bits1: BitSet   = input.toBitSet
    val hotspot = new HotSpot(length)
    val bits2: BitSet   = hotspot.mutate(bits1)
    ! bits1.equals(bits2)
  }


  property("mutateTheFlipOutOfIt") =
    forAll(posNum[Int]) { input: Int =>
      checkOneHotSpotMutation(input)
    }

}