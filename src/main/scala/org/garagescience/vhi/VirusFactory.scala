package org.garagescience.vhi

import scala.collection.immutable.BitSet
import scala.collection.immutable.Vector
import FixedSizeSequence._
import IntegralTypeOps.{toBitSet, toBitSetOption, wrapLong, wrapInt}

object VirusFactory {

  val randomSequenceLength = 28 * 28
  val maxRandomShort = 255
  val randomShort = scala.util.Random
  object CreateFixedSeq1 extends FixedSizeSeqFactory(capacity=randomSequenceLength)

  protected def generateViralSequence: Vector[Int] =
    (for (i <- 1 to randomSequenceLength) yield randomShort.nextInt(maxRandomShort)).toVector

  // make this a higer-order-function? that way, we could pass in the function
  // to generate the sequence if we wanted to change it ...
  def convertToBitSet: Vector[BitSet] = {
    val xs: Vector[Int] = generateViralSequence
    (xs.flatMap(x => toBitSetOption(x))).toVector
  }

  def createViralStrain(sz: Int): SingleStranded = {
    val genes: Vector[BitSet] = convertToBitSet
    val sequence: Vector[Int] = generateViralSequence
    SingleStranded(genes, sequence)
  }

}

trait Virus[U,V] {

  def genes: Vector[U]
  def sequence: Vector[V]
}

case class SingleStranded(val genes: Vector[BitSet],
                          val sequence: Vector[Int])  extends Virus[BitSet, Int] {
  // we need to have some score which
  // shows how similar we are to the e.g. MNIST images

}


