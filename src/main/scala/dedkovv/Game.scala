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

  def roll(pins: Int): Unit = {
    require(pins >= 0 && pins <= 10)

    assert(!isFinished)

    rollInd += 1

    if (frameToPins.contains(frameInd)) {
      val scoresPerFrameSeq = frameToPins(frameInd)

      assert(scoresPerFrameSeq.nonEmpty && (scoresPerFrameSeq.size <= maxNumOfTries))

      val scoresPerFrame = scoresPerFrameSeq.sum

      if (frameInd == lastFrameInd) {
        frameToPins += (frameInd -> (scoresPerFrameSeq :+ pins))
      } else if (scoresPerFrame == maxPins || scoresPerFrameSeq.size == maxNumOfTries) {
        frameInd += 1
        updateFrameToPins(pins)
      } else {
        val rem = maxPins - scoresPerFrameSeq.last

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

  private def isSpare(seq: Seq[Score]): Boolean = seq.size == maxNumOfTries && seq.sum == maxPins
  private def isStrike(seq: Seq[Score]): Boolean = seq.size == 1 && seq.sum == maxPins

  def score(): Int = {
    assert(isFinished)

    var scoreN = 0
    var ind = 0

    val tries = frameToPins.toSeq.flatMap(_._2)

    frameToPins.toSeq.sortBy(_._1).foreach { case (_, scores) =>
      if (isStrike(scores)) {
        scoreN += (tries(ind + 1) + tries(ind + 2) + maxPins)
        ind += 1
      } else if (isSpare(scores)) {
        scoreN += (tries(ind + 2) + maxPins)
        ind += 2
      } else {
        scoreN += (tries(ind) + tries(ind + 1))
        ind += 2
      }
    }

    scoreN
  }

  private def updateFrameToPins(pins: Int): Unit = {
    frameToPins += (frameInd -> List(pins))
  }
}

object Game extends App {
}
