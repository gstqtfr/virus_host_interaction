package org.garagescience.vhi

import scala.collection.immutable.BitSet
import scala.collection.immutable.Vector

object AffinityScore {

  def getAffinity(bits1: BitSet, bits2: BitSet) = {
    def _getAffinity(bits1: BitSet, bits2: BitSet) = {
      (for (idx <- 0 to bits1.size-1)
        yield (bits1(idx) == bits2(idx))).filter(_ == true).length
    }
    if (bits1.size >= bits2.size)
      _getAffinity(bits1, bits2)
    else
      _getAffinity(bits2, bits1)
  }


  def getAffinityBetweenVirusAndImage(x: Vector[BitSet], y: Vector[BitSet]) = {
    val zipped = x zip y
    val singleAffinity: Vector[Int] = zipped.map {
      case (x, y) => getAffinity(x,y)
      case _      => 0
    }
    singleAffinity.foldLeft(0.0)(_ + _) / zipped.length
  }


  def getPopulationAffinity(viruses: Seq[SingleStranded],
                            images: Seq[Vector[BitSet]]): Seq[Double] = {
    viruses.map { v =>
      val perImageAffinity = images.map { img =>
        getAffinityBetweenVirusAndImage(v.genes, img)
      }
      perImageAffinity.foldLeft(0.0)(_ + _) / images.length
    }.toSeq
  }

  def medianAffinity(s: Seq[Double])  = {
    val (lower, upper) = s.sortWith(_<_).splitAt(s.size / 2)
    if (s.size % 2 == 0) (lower.last + upper.head) / 2.0 else upper.head
  }


}
