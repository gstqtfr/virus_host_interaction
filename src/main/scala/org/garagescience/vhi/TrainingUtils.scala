package org.garagescience.vhi

import scala.collection.immutable.{BitSet, Vector}
import IntegralTypeOps._

object TrainingUtils {

  def batch2BitSet(batch_dataset: Seq[Array[Int]]): IndexedSeq[Vector[BitSet]] = {
    def arrayOfInt2BitSet(arr: Array[Int]): Vector[BitSet] =
      arr.flatMap(x => toBitSetOption(x)).toVector

    for (idx <- 0 to batch_dataset.length-1) yield arrayOfInt2BitSet(batch_dataset(idx))
  }
}