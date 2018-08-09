package dedkovv

class Game {
  type FrameInd = Int
  type Score = Int

  val maxPins = 10
  val maxNumOfTry = 2

  var rollInd: Int = -1
  var frameInd: FrameInd = 0
  var frameToPins = Map.empty[FrameInd, Seq[Score]]

  var scoreN = 0

  def roll(pins: Int): Unit = {
    require(pins >= 0 && pins <= 10)

    rollInd += 1
    if (frameToPins.contains(frameInd)) {
      val scoresPerFrameSeq = frameToPins(frameInd)

      assert(scoresPerFrameSeq.nonEmpty && scoresPerFrameSeq.size <= maxNumOfTry)

      val scoresPerFrame = scoresPerFrameSeq.sum

      if (scoresPerFrame == maxPins || scoresPerFrameSeq.size == maxNumOfTry) {
        frameInd += 1
        updateFrameToPins(pins)
      } else {
        val tail = maxPins - scoresPerFrameSeq.last

        assert(pins <= tail)

        frameToPins += (frameInd -> (scoresPerFrameSeq :+ pins))
      }
    } else {
      updateFrameToPins(pins)
    }
  }

  def score(): Int = ???

  private def updateFrameToPins(pins: Int): Unit = {
    frameToPins += (frameInd -> List(pins))
  }
}

object Game extends App {
}
