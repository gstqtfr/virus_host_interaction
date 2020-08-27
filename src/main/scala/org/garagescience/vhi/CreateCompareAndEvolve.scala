package org.garagescience.vhi

import scala.collection.immutable.BitSet
import scala.collection.immutable.Vector


object CreateCompareAndEvolve {

  import PimpedMnistDataFetcher._
  import VirusFactory._
  import IntegralTypeOps._
  import AffinityScore._

  // could always implement this as a stream, i suppose ...
  // move this to a trait?
  private def getBatch(sz: Int=128,
                       labels: IndexedSeq[Int],
                       data: IndexedSeq[Array[Int]]) = {
    // generate a list of random integers from 0 .. data.length
    val xs = for (i <- 1 to sz) yield scala.util.Random.nextInt(data.length)

    val shuffled_labels = for (elem <- xs) yield labels(elem)
    val shuffled_data   = for (elem <- xs) yield data(elem)

    (shuffled_labels, shuffled_data)
  }

  // we'll need to convert both types to BitSets b/4 we call this
  // ... assuming these two bitsets are the same length?
  //private
  // superceded by AffinityScore, but we can use this outline to Do Stuff
  // to the whole vector of virus/image ...
  def getAffinity(nonself: Vector[BitSet], self: Vector[BitSet]) = {

    val affinities = for (idx <- 0 to nonself.length-1)
      yield (nonself(idx) & self(idx)).count(_=>true)

    affinities
  }




  val baseDirectory = "/home/johnny/Data/mnist/MNIST/raw"

  val (testLabels, trainingLabels, testImages, trainingImages) =
    LoadMnist.getMnistImageData(baseDirectory)


  // so we need to create a viral population

  private val sizeof_viral_population = 500
  // don't really need this ...
  // private val vrialGeneSequenceLength = 28 * 28

  val viralPopulation = for (i <- 1 to sizeof_viral_population) yield createViralStrain
  // okay, so this creates: IndexedSeq[org.garagescience.vhi.SingleStranded] = Vector(SingleStranded ...
  // jolly good, onwards ...






}