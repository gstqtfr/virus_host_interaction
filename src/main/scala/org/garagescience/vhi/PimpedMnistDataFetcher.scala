package org.garagescience.vhi

import org.nd4j.linalg.dataset.api.DataSet
import org.deeplearning4j.datasets.fetchers.MnistDataFetcher
import org.deeplearning4j.datasets.mnist.MnistManager
import org.nd4j.linalg.api.buffer.util.DataTypeUtil
import org.nd4j.linalg.api.ndarray.INDArray

// stolen from:
// https://github.com/nelson-wu/scala-mnist-test/blob/master/src/main/scala/me/nelsonwu/MnistLoader.scala
// with bits i've mucked about with ...

// the data can be found under: /home/johnny/Data/mnist/MNIST/raw

object PimpedMnistDataFetcher {

  import java.io.{BufferedInputStream, FileInputStream}
  import java.util.zip.GZIPInputStream
  import org.nd4j.linalg.factory.Nd4j

  object LoadMnist {


    private def gzipInputStream(s: String) = new GZIPInputStream(new BufferedInputStream(new FileInputStream(s)))

    private def read32BitInt(i: GZIPInputStream) = i.read() * 16777216 /*2^24*/ + i.read() * 65536 /*2&16*/ + i.read() * 256 /*2^8*/ + i.read()


    /**
     *
     * @param baseDirectory the directory for the standard mnist images, file names are assumed
     */
    def getMnistImageData(baseDirectory: String) = {
      // let's get rid of this ...
      // DataTypeUtil.setDTypeForContext("double")
      val testLabels = readLabels(s"$baseDirectory/t10k-labels-idx1-ubyte.gz")
      val trainingLabels = readLabels(s"$baseDirectory/train-labels-idx1-ubyte.gz")
      val testImages = readImages(s"$baseDirectory/t10k-images-idx3-ubyte.gz")
      val trainingImages = readImages(s"$baseDirectory/train-images-idx3-ubyte.gz")
      //(testLabels, trainingLabels, testImages.map(_.div(255)), trainingImages.map(_.div(255)))
      (testLabels, trainingLabels, testImages, trainingImages)
    }

    /**
     *
     * @param filepath the full file path the labels file
     * @return
     */
    def readLabels(filepath: String) = {

      DataTypeUtil.setDTypeForContext("double")
      val g = gzipInputStream(filepath)
      val magicNumber = read32BitInt(g) //currently not used for anything, as assumptions are made
      val numberOfLabels = read32BitInt(g)
      1.to(numberOfLabels).map(_ => g.read())
    }

    /**
     *
     * @param filepath the full file path of the images file
     * @return
     */
    def readImages(filepath: String) = {
      // hmm ... let's think about this 'un, not sure this should be here At All ...
      // DataTypeUtil.setDTypeForContext("double")
      val g = gzipInputStream(filepath)
      val magicNumber = read32BitInt(g) //currently not used for anything, as assumptions are made
      val numberOfImages = read32BitInt(g)
      val imageSize = read32BitInt(g) * read32BitInt(g) //cols * rows
      // unfortunately, this *requires* floaty-point numbers, so we can either:
      // a) deal with conversion at a later point, or
      // b) write the fucking thing myself ...
      // (1 to numberOfImages).map(_ => Nd4j.create((1 to imageSize).map(_ => g.read().toDouble).toArray))
      (1 to numberOfImages).map(_ => (1 to imageSize).map(_ => g.read().toInt).toArray)
    }
  }

}




