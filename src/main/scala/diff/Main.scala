package diff

import javax.imageio.ImageIO
import java.io.File
import java.awt.image.BufferedImage

import scala.collection.mutable.ArrayBuffer

import DiffFinder.findDiffs

import DiffImageWriter.writeDiffToImage

object Main {
  def main(args: Array[String]):Unit = {
    args.foreach(println)
    val file1 = new File(args(0))
    val file2 = new File(args(1))

    if(!file1.exists()) {
      println(s"File ${args(0)} doesn't exist.")
      System.exit(1)
    }
    
    if(!file2.exists()) {
      println(s"File ${args(1)} doesn't exist.")
      System.exit(1)
    }

    val img1 = ImageIO.read(file1)
    val img2 = ImageIO.read(file2)
    

    if(img1 == null) {
      println(s"Image ${args(0)} couldn't be loaded.")
      System.exit(1)
    }
    
    if(img2 == null) {
      println(s"Image ${args(1)} couldn't be loaded.")
      System.exit(1)
    }

    if(img1.getWidth != img2.getWidth || img1.getHeight != img2.getHeight) {
      println("Images must have the same size.")
      System.exit(1)
    }

    val resultLines = findDiffs(img1, img2)

    val diffImage = writeDiffToImage(img1, img2, resultLines)
  
    ImageIO.write(diffImage, "png", new File("diff.png"))
  }
}