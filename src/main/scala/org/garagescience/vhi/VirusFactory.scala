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

  def createViralStrainWithSeq: SingleStrandedWithSeq = {
    val genes: Vector[BitSet] = convertToBitSet
    val sequence: Vector[Int] = generateViralSequence
    SingleStrandedWithSeq(genes, sequence)
  }

  def createViralStrain: SingleStranded = {
    val genes: Vector[BitSet] = convertToBitSet
    SingleStranded(genes)
  }

}

trait Virus[U] {

  def genes: Vector[U]
}

case class SingleStrandedWithSeq(val genes: Vector[BitSet],
                                 val sequence: Vector[Int])  extends Virus[BitSet]

case class SingleStranded(val genes: Vector[BitSet]) extends Virus[BitSet]


