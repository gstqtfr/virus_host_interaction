package org.garagescience.vhi

import scala.collection.immutable.BitSet
import scala.collection.immutable.Vector
import scala.collection.mutable.ArraySeq

object ViralSubpopulations {

  import PimpedMnistDataFetcher._
  import VirusFactory._
  import IntegralTypeOps._
  import AffinityScore._
  import TrainingUtils._

  def createDatasetAccordingToLabel(label: Int, labels: Seq[Int], data: Seq[Array[Int]]): Seq[Array[Int]] = {
    labels.zipWithIndex.filter { case (digit, idx) => digit == label }.
      map { case (digit, idx) => data(idx) }
  }

  def createSubPopulations(subPopulationSize: Int=100): Array[Array[SingleStranded]] = {
    // create our subpopulations ...
    val subpops = Array.ofDim[SingleStranded](10, subPopulationSize)
    // ... then initialise them (very unScala, somehow, but it works just fine) ...
    for {i <- 0 to 9
         j <- 0 to subPopulationSize-1}
      subpops(i)(j) = createViralStrain
    //subpops.toArraySeq
    subpops
  }

  def getBatch(sz: Int = 128,
               data: Seq[Array[Int]]) = {
    // generate a list of random integers from 0 .. data.length
    val xs = for (i <- 1 to sz) yield scala.util.Random.nextInt(data.length)
    // go get that data ...
    for (elem <- xs) yield data(elem)
  }


  // where to find our images
  // TODO: need to get this frm the command line, make it a default
  val baseDirectory = "/home/johnny/Data/mnist/MNIST/raw"

  // size of the viral pop
  val sizeof_viral_population = 1000
  // how long we run the main training section of the algo. for
  val iterations = 500

  // load up our data
  val (testLabels, trainingLabels, testImages, trainingImages) =
    LoadMnist.getMnistImageData(baseDirectory)

  def main(args: Array[String]): Unit = {


    // we're going to be using subpopulations for viral species specialisation
    // so we need to enusre these guys are all in subpopulations themselves ...

    val datasetsByLabel = for (digit <- 0 to 9)
      yield createDatasetAccordingToLabel(digit, trainingLabels, trainingImages)

    // now we need to create a viral population, consisting of subpopulations, which
    // we adapt to the particular digit, thus enabling speciation ...

    // create our population of viruses
    // this is a var! a fucking *var*!!! WTActualF?!?!!?
    val viralPopulation = createSubPopulations()

    for (iteration <- 1 to iterations) {
      // do stuff here ...
      for (label <- 0 to 9) {
        // here's out batch of labels & images

        // needs to be the datatsetsByLabel collection here ...
        val _batch_images = getBatch(data = datasetsByLabel(label))

        // transform the images into bitsets ...
        val batch_images = batch2BitSet(_batch_images)

        val populationAffinities = getPopulationAffinity(viralPopulation(label).toIndexedSeq, batch_images)

        // get the average population affinity ...
        val meanAffinity = populationAffinities.foldLeft(0.0)(_ + _) / populationAffinities.length
        println(s"$iteration : $label : $meanAffinity")

        val medianPopulationAffinity = medianAffinity(populationAffinities)

        // this gets all the viral particles which have higher affinity than the
        // median affinity.
        val higherAffinityPopulation = populationAffinities.zipWithIndex.
          filter { case (a: Double, b: Int) => a >= medianPopulationAffinity }.
          map { case (a: Double, index: Int) => viralPopulation(label)(index) }.toArray

        // this creates a new population by taking the higher afinity subpop
        // and then creating new mutant versions of the viral pop.
        viralPopulation(label) =
          higherAffinityPopulation ++ higherAffinityPopulation.map { case v => SingleStranded(v.mutateHotspots(v.genes)) }


      }


    }

  }


}