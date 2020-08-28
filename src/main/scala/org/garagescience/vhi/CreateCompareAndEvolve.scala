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
  def getBatch(sz: Int = 128,
               labels: Seq[Int],
               data: Seq[Array[Int]]) = {
    // generate a list of random integers from 0 .. data.length
    val xs = for (i <- 1 to sz) yield scala.util.Random.nextInt(data.length)

    val shuffled_labels = for (elem <- xs) yield labels(elem)
    val shuffled_data = for (elem <- xs) yield data(elem)

    (shuffled_labels, shuffled_data)
  }

  def batch2BitSet(batch_dataset: Seq[Array[Int]]): IndexedSeq[Vector[BitSet]] = {
    def arrayOfInt2BitSet(arr: Array[Int]): Vector[BitSet] =
      arr.flatMap(x => toBitSetOption(x)).toVector

    //batch_dataset.flatMap(arrayOfInt2BitSet(_))

    for (idx <- 0 to batch_dataset.length-1) yield arrayOfInt2BitSet(batch_dataset(idx))
  }

  // we'll need to convert both types to BitSets b/4 we call this
  // ... assuming these two bitsets are the same length?
  //private
  // superceded by AffinityScore, but we can use this outline to Do Stuff
  // to the whole vector of virus/image ...


  /*

  def getVectorAffinities(v1: Seq[BitSet], v2: Seq[BitSet]) = {
    val affinities = for (idx <- 0 to v1.length-1)
      yield (getAffinity(v1(idx), v2(idx)))
    affinities
  }

  val zippedVirusAndImage1 = viralPopulation(0).genes zip batch_images(0)

  */


  /*
  val singleAffinity = zippedVirusAndImage1.map {
     |     case (x, y) => getAffinity(x,y)
     |     case _      =>
     | }

  val zippedVirusAndImage1 = viralPopulation(0).genes zip batch_images(0)

   */


  def getAffinityBetweenVirusAndImage(x: Vector[BitSet], y: Vector[BitSet]) = {
    val zipped = x zip y
    val singleAffinity: Vector[Int] = zipped.map {
      case (x, y) => getAffinity(x,y)
      case _      => 0
    }
    singleAffinity.foldLeft(0.0)(_ + _) / zipped.length
  }


  def getPopulationAffinity(viruses: Seq[SingleStranded],
                            images: Seq[Vector[BitSet]]) = {
    viruses.map { v =>
      val perImageAffinity = images.map { img =>
        getAffinityBetweenVirusAndImage(v.genes, img)
      }
      perImageAffinity.foldLeft(0.0)(_ + _) / images.length
    }
  }

  // viralPopulation.map { v => getAffinityBetweenVirusAndImage(v.genes, batch_images(0)) }

  /* grown-up version:

  viralPopulation.map { v =>
     val perImageAffinity = batch_images.map{ img =>
            getAffinityBetweenVirusAndImage(v.genes, img)
          }
      perImageAffinity.foldLeft(0.0)(_ + _) / batch_images.length
      }
   */


  // SingleStranded? maybe the parent class?
  /*
  def getMeanAffinityBetweenViralPopulationAndImages(viralPopulation: Vector[SingleStranded],
                                                     images: Vector[BitSet]) = {
    val populationAffinities = for (v <- viralPopulation) {
      val affinities = for (img <- images) {
        yield getAffinityBetweenVirusAndImage(v.genes, img)
      }
    }
    populationAffinities
  }
   */

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
    val viralPopulation = for (i <- 1 to sizeof_viral_population) yield createViralStrain
    // okay, so this creates: IndexedSeq[org.garagescience.vhi.SingleStranded] = Vector(SingleStranded ...
    // jolly good, onwards ...


    // all very iterative this, lots of for loops; well, as longas it
    // works, i'm happy ...
    for (iteration <- 1 to iterations) {

      // here's out batch of labels & images
      val (batch_labels, _batch_images) = getBatch(labels = trainingLabels, data = trainingImages)

      // so, we need to transform the images into bitsets ...
      val batch_images = batch2BitSet(_batch_images)

      // do we need a groovy for-comp here?

      for (v <- viralPopulation) {
        for (img <- batch_images) {
          yield getAffinityBetweenVirusAndImage(v, img)
        }
      }

/*

      for (v <- viralPopulation) {
        for (img <- batch_images)
          // now we need to get (& store) the affinity
          yield getAffinity(v.genes, img)
      }

*/


    }

  }
}