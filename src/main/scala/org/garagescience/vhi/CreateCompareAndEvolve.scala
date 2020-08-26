package org.garagescience.vhi

import scala.collection.immutable.BitSet
import scala.collection.immutable.Vector


object CreateCompareAndEvolve {

  private val randomInt = scala.util.Random

  // could always implement this as a stream, i suppose ...
  private def getBatch(sz: Int=128, labels: IndexedSeq[Int],
                       data: IndexedSeq[Array[Int]]) = {
    // generate a list of random integers from 0 .. data.length
    val xs = for (i <- 1 to sz) yield scala.util.Random.nextInt(data.length)

    val shuffled_labels = for (elem <- xs) yield labels(elem)
    val shuffled_data   = for (elem <- xs) yield data(elem)

    (shuffled_labels, shuffled_data)
  }

  import PimpedMnistDataFetcher._

  val baseDirectory = "/home/johnny/Data/mnist/MNIST/raw"

  val (testLabels, trainingLabels, testImages, trainingImages) =
    LoadMnist.getMnistImageData(baseDirectory)

}