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

  // we need to find out the largest bitset


}
