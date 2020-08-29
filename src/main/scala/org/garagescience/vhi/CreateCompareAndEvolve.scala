package org.garagescience.vhi

import scala.collection.immutable.BitSet
import scala.collection.immutable.Vector

object CreateCompareAndEvolve {

  import PimpedMnistDataFetcher._
  import VirusFactory._
  import IntegralTypeOps._
  import AffinityScore._
  import TrainingUtils._

  // could always implement this as a stream, i suppose ...
  // move this to a trait?
  def getBatch(sz: Int = 128,
               labels: Seq[Int],
               data: Seq[Array[Int]]) = {
    // generate a list of random integers from 0 .. data.length
    val xs = for (i <- 1 to sz) yield scala.util.Random.nextInt(data.length)

    val shuffled_labels = for (elem <- xs) yield labels(elem)
    val shuffled_data = for (elem <- xs) yield data(elem)

    (shuffled_labels, shuffled_data)
  }

  // where to find our images
  val baseDirectory = "/home/johnny/Data/mnist/MNIST/raw"
  // size of the viral pop
  val sizeof_viral_population = 500
  // how long we run the main training section of the algo. for
  val iterations = 500

  def main(args: Array[String]): Unit = {

    // load up our data
    val (testLabels, trainingLabels, testImages, trainingImages) =
      LoadMnist.getMnistImageData(baseDirectory)

    // create our population of viruses
    var viralPopulation: Seq[org.garagescience.vhi.SingleStranded] =
      for (i <- 1 to sizeof_viral_population) yield createViralStrain

    // all very iterative this, lots of for loops; well, as longas it
    // works, i'm happy ...
    for (iteration <- 1 to iterations) {

      // here's out batch of labels & images
      val (batch_labels, _batch_images) = getBatch(labels = trainingLabels, data = trainingImages)

      // so, we need to transform the images into bitsets ...
      val batch_images = batch2BitSet(_batch_images)

      // do we need a groovy for-comp here?

      val populationAffinities = getPopulationAffinity(viralPopulation, batch_images)

      // get the average population affinity ...
      val meanAffinity = populationAffinities.foldLeft(0.0)(_ + _) / populationAffinities.length
      println(s"$iteration :  $meanAffinity")

      // populationAffinities.foreach { println(_) }

      val medianPopulationAffinity = medianAffinity(populationAffinities)

      // this gets all the viral particles which have higher affinity than the
      // median affinity. now i need to mutate 'em ...
      val higherAffinityPopulation = populationAffinities.zipWithIndex.
        filter { case (a: Double, b: Int) => a >= medianPopulationAffinity }.
        map { case (a: Double, index: Int) => viralPopulation(index) }

      // this creates a new population by taking the higher afinity subpop
      // and then creating new mutant versions of the viral pop.
      // but let's finish this up tomorrow ...
      viralPopulation = higherAffinityPopulation ++ higherAffinityPopulation.map { case v =>
        SingleStranded(v.mutateHotspots(v.genes)) }

      // question is: how do we assign this to our population? as we
      // go around the loop? mutable collection?
    }

  }
}