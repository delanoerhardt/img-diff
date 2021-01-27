package diff.finder

import java.awt.image.BufferedImage

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArraySeq

import diff.{LineState, LineUnchanged, LineAdded, LineRemoved}

class FullShaker extends DiffFinder {

  def findDiffs(img1: BufferedImage, img2: BufferedImage): ArrayBuffer[LineState] = {
    val list = for {i <- (0 to img1.getHeight)} yield new Section(i)

    val buffer1 = new Array[Int](img1.getWidth)
    val buffer2 = new Array[Int](img2.getWidth)

    var currentPos = 0
    var changedPos = 0
    for(i <- (0 until img1.getHeight)) {
      img1.getRGB(0, list(i).originalBot, img1.getWidth, 1, buffer1, 0, 0)

      currentPos = 0
      while(currentPos < img2.getHeight) {
        changedPos = (list(i).originalBot + currentPos) % img2.getHeight
        img2.getRGB(0, changedPos, img2.getWidth, 1, buffer2, 0, 0)

        if ((buffer1, buffer2).zipped.map(_ == _).filter(_ == false).isEmpty) {
          list(i).changedTop = changedPos
          list(i).changedBot = changedPos
          currentPos = img2.getHeight
        }
        currentPos += 1
      }
    }

    for(section <- list) {
      var changed = true
      while(changed) {
        changed = false
        if(section.originalBot > 0 && section.changedBot > 0)  {
          img1.getRGB(0, section.originalBot-1, img1.getWidth, 1, buffer1, 0, 0)
          img2.getRGB(0, section.changedBot-1, img2.getWidth, 1, buffer2, 0, 0)
          if ((buffer1, buffer2).zipped.map(_ == _).filter(_ == false).isEmpty) {
            section.changedBot -= 1
            section.originalBot -= 1
            changed = true
          }
        }

        if(section.originalTop < img1.getHeight - 1 && section.changedTop < img2.getHeight - 1) {
          img1.getRGB(0, section.originalTop+1, img1.getWidth, 1, buffer1, 0, 0)
          img2.getRGB(0, section.changedTop+1, img2.getWidth, 1, buffer2, 0, 0)
          if ((buffer1, buffer2).zipped.map(_ == _).filter(_ == false).isEmpty) {
            section.changedTop += 1
            section.originalTop += 1
            changed = true
          }
        }
      }
    }

    var j = 0
    for(i <- (0 until img1.getHeight)) {
      if(list(i).changedBot != -1) {
        j = 0
        while(j < img1.getHeight) {
          if(i != j && list(j).changedBot != -1) {
            val tangentOriginalAbove = list(i).originalBot <= list(j).originalBot && list(i).originalTop >= list(j).originalBot
            val tangentOriginalBelow = list(i).originalBot <= list(j).originalTop && list(i).originalTop >= list(j).originalTop
            val tangentChangedAbove = list(i).changedBot <= list(j).changedBot && list(i).changedTop >= list(j).changedBot
            val tangentChangedBelow = list(i).changedBot <= list(j).changedTop && list(i).changedTop >= list(j).changedTop

            if((list(i).originalBot <= list(j).originalBot && list(i).originalTop >= list(j).originalTop) ||
               (list(i).changedBot <= list(j).changedBot && list(i).changedTop >= list(j).changedTop)) {
              list(j).changedBot = -1
            } else if((list(j).originalBot <= list(i).originalBot && list(j).originalTop >= list(i).originalTop) ||
                      (list(j).changedBot <= list(i).changedBot && list(j).changedTop >= list(i).changedTop)) {
              list(i).changedBot = -1
              j = img1.getHeight
            } else {
              var diff = 0
              var changeTop = 0
              var changeBot = 0
              if(tangentOriginalAbove) {
                diff = list(i).originalTop - list(j).originalBot + 1
                changeTop = -diff/2
                changeBot = diff/2 + diff - (diff/2 + diff/2)
                list(i).originalTop += changeTop
                list(j).originalBot += changeBot
                list(i).changedTop += changeTop
                list(j).changedBot += changeBot
              } else if(tangentOriginalBelow) {
                diff = list(j).originalTop - list(i).originalBot + 1
                changeTop = -diff/2
                changeBot = diff/2 + diff - (diff/2 + diff/2)
                list(j).originalTop += changeTop
                list(i).originalBot += changeBot
                list(j).changedTop += changeTop
                list(i).changedBot += changeBot
              } else if(tangentChangedAbove) {
                diff = list(i).changedTop - list(j).changedBot + 1
                changeTop = -diff/2
                changeBot = diff/2 + diff - (diff/2 + diff/2)
                list(i).originalTop += changeTop
                list(j).originalBot += changeBot
                list(i).changedTop += changeTop
                list(j).changedBot += changeBot
              } else if(tangentChangedBelow) {
                diff = list(j).changedTop - list(i).changedBot + 1
                changeTop = -diff/2
                changeBot = diff/2 + diff - (diff/2 + diff/2)
                list(j).originalTop += changeTop
                list(i).originalBot += changeBot
                list(j).changedTop += changeTop
                list(i).changedBot += changeBot
              }
            }
          }
          j += 1
        }
      }
    }

    var list2 = list.filter(_.changedBot != -1).distinct.toList

    var i = 0
    while(i < list2.length - 1) {
      if(list2(i).originalBot > list2(i + 1).originalBot || list2(i).changedBot == -1) {
        val firstCopy = list2(i).copyTo(new Section(0))
        list2(i + 1).copyTo(list2(i))
        firstCopy.copyTo(list2(i + 1))
        i -= 2
      }
      i += 1
    }

    i = 0
    j = 1
    while(i + j < list2.length) {
      if(list2(i).originalBot == list2(i + j).originalBot && list2(i).originalTop == list2(i + j).originalTop)
        list2(i + j).changedBot = -1
      else {
        i += j
        j = 0
      }
      j += 1
    }

    list2 = list2.filter(_.changedBot != -1)

    val resultLines = ArrayBuffer[LineState]()

    var yImg1 = 0
    var yImg2 = 0
    for(section <- list2) {
      if(yImg1 < section.originalBot) {
        (yImg1 until section.originalBot).foreach(_=> resultLines += LineRemoved)
      }
      if(yImg2 < section.changedBot) {
        (yImg2 until section.changedBot).foreach(_=> resultLines += LineAdded)
      }
      (section.originalBot to section.originalTop).foreach(_=> resultLines += LineUnchanged)
      yImg1 = section.originalTop + 1
      yImg2 = section.changedTop + 1
    }

    resultLines
  }

  class Section(var originalBot: Int) {
    var originalTop: Int = originalBot
    var changedTop: Int = -1
    var changedBot: Int = -1

    def copyTo(section: Section): Section = {
      section.originalBot = this.originalBot
      section.originalTop = this.originalTop
      section.changedBot = this.changedBot
      section.changedTop = this.changedTop
      section
    }
  }
}