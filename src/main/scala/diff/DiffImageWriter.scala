package diff

import java.awt.image.BufferedImage

import scala.collection.mutable.ArrayBuffer

object DiffImageWriter {

  val COLOR_FADE_BY = 0.9F
  val COLOR_INCREASE_BY = 1.2F

  def tintLine(buffer: Array[Int], weights: Tuple3[Float, Float, Float]): Array[Int] = {
    for(pixel <- buffer) yield {
      val red   = (pixel & 0x00FF0000) >> 16
      val green = (pixel & 0x0000FF00) >> 8
      val blue  = pixel & 0x000000FF
      ((red * weights._1).toInt.min(255) << 16) | ((green * weights._2).toInt.min(255) << 8) | (blue * weights._3).toInt.min(255)
    }
  }

  def writeDiffToImage(img1: BufferedImage, img2: BufferedImage, resultLines: ArrayBuffer[LineState]): BufferedImage = {
    val diffImage = new BufferedImage(img1.getWidth, resultLines.length, BufferedImage.TYPE_INT_RGB)

    val buffer1 = new Array[Int](img1.getWidth)
    val buffer2 = new Array[Int](img2.getWidth)

    var yImg1 = 0
    var yImg2 = 0
    for(i <- (0 until resultLines.length)) {
      resultLines(i) match {
      case LineUnchanged =>
        img2.getRGB(0, yImg2, img1.getWidth, 1, buffer1, 0, 0)
        diffImage.setRGB(0, i, img1.getWidth, 1, buffer1, 0, 0)
        yImg1 += 1
        yImg2 += 1
      case LineAdded =>
        img2.getRGB(0, yImg2, img2.getWidth, 1, buffer2, 0, 0)
        val tintedLine = tintLine(buffer2, (COLOR_FADE_BY, COLOR_INCREASE_BY, COLOR_FADE_BY))
        diffImage.setRGB(0, i, img1.getWidth, 1, tintedLine, 0, 0)
        yImg2 += 1
      case LineRemoved =>
        img1.getRGB(0, yImg1, img1.getWidth, 1, buffer1, 0, 0)
        val tintedLine = tintLine(buffer1, (COLOR_INCREASE_BY, COLOR_FADE_BY, COLOR_FADE_BY))
        diffImage.setRGB(0, i, img1.getWidth, 1, tintedLine, 0, 0)
        yImg1 += 1
      }
    }

    diffImage
  }
}