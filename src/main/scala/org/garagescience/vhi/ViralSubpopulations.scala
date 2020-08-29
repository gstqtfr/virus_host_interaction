package org.garagescience.vhi

import scala.collection.immutable.BitSet
import scala.collection.immutable.Vector

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

  def createSubPopulations(subPopulationSize: Int=50): Seq[Array[SingleStranded]] = {
    // create our subpopulations ...
    val subpops = Array.ofDim[SingleStranded](10, subPopulationSize)
    // ... then initialise them (very unScala, somehow, but it works just fine) ...
    for {i <- 0 to 9
         j <- 0 to subPopulationSize-1}
      subpops(i)(j) = createViralStrain
    subpops.toSeq
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

  // we're going to be using subpopulations for viral species specialisation
  // so we need to enusre these guys are all in subpopulations themselves ...

  val datatsetsByLabel = for (digit <- 0 to 9)
    yield createDatasetAccordingToLabel(digit, trainingLabels, trainingImages)

  // now we need to create a viral population, consisting of subpopulations, which
  // we adapt to the particular digit, thus enabling speciation ...

  // create our population of viruses
  // this is a var! a fucking *var*!!! WTActualF?!?!!?
  var viralPopulation = createSubPopulations()

  for (iteration <- 1 to iterations) {
    // do stuff here ...
    for (label <- 0 to 0) {
      // here's out batch of labels & images
      val batch_images = getBatch(data = trainingImages)

    }





  }



}