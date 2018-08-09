package dedkovv

class Game(val lastFrameInd: Int = 9) {
  type FrameInd = Int
  type Score = Int

  val maxPins = 10
  val maxNumOfTries = 2
  val maxNumOfTriesInLastFrame = 3

  var rollInd: Int = -1
  var frameInd: FrameInd = 0
  var frameToPins = Map.empty[FrameInd, Seq[Score]]
  var isFinished = false

  var scoreN = 0

  def roll(pins: Int): Unit = {
    require(pins >= 0 && pins <= 10)

    assert(!isFinished)

    rollInd += 1

    if (frameToPins.contains(frameInd)) {
      val scoresPerFrameSeq = frameToPins(frameInd)

      assert(scoresPerFrameSeq.nonEmpty && (scoresPerFrameSeq.size <= maxNumOfTries))

      val scoresPerFrame = scoresPerFrameSeq.sum

      if (frameInd == lastFrameInd) {
        val rem = remainder(scoresPerFrameSeq)

        assert(rem == 0 || pins <= rem)

        frameToPins += (frameInd -> (scoresPerFrameSeq :+ pins))
      } else if (scoresPerFrame == maxPins || scoresPerFrameSeq.size == maxNumOfTries) {
        frameInd += 1
        updateFrameToPins(pins)
      } else {
        val rem = remainder(scoresPerFrameSeq)

        assert(pins <= rem)

        frameToPins += (frameInd -> (scoresPerFrameSeq :+ pins))
      }
    } else {
      updateFrameToPins(pins)
    }

    if (frameInd == lastFrameInd) {
      val scoresPerFrameSeq = frameToPins(frameInd)
      if (scoresPerFrameSeq.size == maxNumOfTriesInLastFrame ||
        scoresPerFrameSeq.size == 2 && scoresPerFrameSeq.sum < maxPins) {
        isFinished = true
      }
    }
  }

  def score(): Int = ???

  private def updateFrameToPins(pins: Int): Unit = {
    frameToPins += (frameInd -> List(pins))
  }

  private def remainder(scoresPerFrameSeq: Seq[Int]): Int = {
    maxPins - scoresPerFrameSeq.last
  }

}

object Game extends App {
}
