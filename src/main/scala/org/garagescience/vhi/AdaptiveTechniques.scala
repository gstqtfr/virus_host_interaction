package org.garagescience.vhi

import scala.util.Random
import scala.collection.immutable.BitSet
import BitSetOps._

// this type parameter infers that we're going to include
// the implicit conversions between T & BitSet, which is a
// fair-enough assumption ...

object AdaptiveTechniques {

  trait MutationOps[T] {

    protected val random: Random = new scala.util.Random

    def mutate(bits: BitSet): BitSet
  }

  class HotSpot[T](protected val length: Int) extends MutationOps[T] {

    // this looks good
    def mutate(bits: BitSet): BitSet = {

      // get our hotspot
      val hotspot = random.nextInt(length)

      // select & modify the site in our bitset
      bits ^ BitSet(hotspot)
    }

  }

  // TODO: JKK: not that we need it for this project, but a
  // TODO: JKK: somatic hypermutation op would be good here ...

}