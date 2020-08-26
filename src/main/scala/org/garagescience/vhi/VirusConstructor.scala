package org.garagescience.vhi

import scala.collection.immutable.BitSet
import FixedSizeSequence._
import IntegralTypeOps.{toBitSet, wrapLong, wrapInt}

object VirusConstructor {

  val randomSequenceLength = 28 * 28
  val maxRandomShort = 255
  val randomShort = scala.util.Random

  def generateViralSequence =
    for (i <- 1 to randomSequenceLength) yield randomShort.nextInt(maxRandomShort)


}