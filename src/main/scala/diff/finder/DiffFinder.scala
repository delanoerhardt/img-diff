package diff.finder

import java.awt.image.BufferedImage

import scala.collection.mutable.ArrayBuffer

import diff.{LineState, LineUnchanged, LineAdded, LineRemoved}

trait DiffFinder {
  def findDiffs(img1: BufferedImage, img2: BufferedImage): ArrayBuffer[LineState]
}