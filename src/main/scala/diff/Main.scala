package diff

import javax.imageio.ImageIO
import java.io.File
import java.awt.image.BufferedImage

import scala.collection.mutable.ArrayBuffer

import DiffImageWriter.writeDiffToImage

import diff.finder._

object Main {
  def main(args: Array[String]):Unit = {
    if(args.length < 2) {
      println("Need the filename of two images as arguments")
      System.exit(1)
    }

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

    if(img1.getWidth != img2.getWidth) {
      println("Images must have the same width.")
      System.exit(1)
    }

    var finderAlgorithm: DiffFinder = null

    if(args.length > 2)
       finderAlgorithm = args(2) match {
        case "iterative" => new Iterative()
        case "i" => new Iterative()
        case "random" => new RandomShaker()
        case "r" => new RandomShaker()
        case "full" => new FullShaker()
        case "f" => new FullShaker()
        case _ => null
      }
     else 
      finderAlgorithm = new RandomShaker()

    val resultLines = finderAlgorithm.findDiffs(img1, img2)

    val diffImage = writeDiffToImage(img1, img2, resultLines)
  
    ImageIO.write(diffImage, "png", new File("diff.png"))
  }
}