// import org.deeplearning4j.datasets.iterator.BaseDatasetIterator

package org.garagescience.vhi

import org.deeplearning4j.datasets.iterator.impl.MnistDataSetIterator


object ReadMNIST  {

  def getTrainData(batchSize: Int,
                           seed: Int = 42) = new MnistDataSetIterator(batchSize, true, seed)

  def getTestData(batchSize: Int,
                          seed: Int = 42) = new MnistDataSetIterator(batchSize, false, seed)

  // Input image dimensions
  val numberOfRows = 28
  val numberOfColumns = 28

  val numberOfOutputClasses = 10

  // Hyper parameters
  val batchSize = 128
  val randomSeed = 42

/*
  def main(args: Array[String]) = {
    println ("Loading data")

    // Data
    val trainingSet = getTrainData (batchSize, randomSeed)
    val testSet = getTestData (batchSize, randomSeed)

    println ("Loaded data. See ya!")
  }

 */

}