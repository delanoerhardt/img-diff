package diff

import java.awt.image.BufferedImage

import scala.collection.mutable.ArrayBuffer

object DiffFinder {
  def iter(yImg1: Int, yImg2: Int, maxY: Int, buffers: Tuple2[Array[Int], Array[Int]], images: Tuple2[BufferedImage, BufferedImage]): Int = {
    if(yImg2 >= maxY) -1
    else {
      images._1.getRGB(0, yImg1, images._1.getWidth, 1, buffers._1, 0, 0)
      images._2.getRGB(0, yImg2, images._2.getWidth, 1, buffers._2, 0, 0)
      buffers.zipped.map(_ == _).filter(_ == false).isEmpty match {
        case true => yImg2
        case false => iter(yImg1, yImg2+1, maxY, buffers, images)
      }
    }
  }

  def findDiffs(img1: BufferedImage, img2: BufferedImage): ArrayBuffer[LineState] = {
    val resultLines = ArrayBuffer[LineState]()

    val buffer1 = new Array[Int](img1.getWidth)
    val buffer2 = new Array[Int](img2.getWidth)

    var yImg2: Int = 0
    for(yImg1 <- (0 until img1.getHeight)) {
      val nextYImg2 = iter(yImg1, yImg2, img2.getHeight, (buffer1, buffer2), (img1, img2))

      nextYImg2 match {
        case y if y == yImg2 =>
          resultLines += LineUnchanged
          yImg2 += 1
        case y if y > yImg2 =>
          (yImg2 until nextYImg2).foreach(_ => resultLines += LineAdded)
          resultLines += LineUnchanged
          yImg2 = nextYImg2 + 1
        case _ => resultLines += LineRemoved
      }
    }

    resultLines
  }
}