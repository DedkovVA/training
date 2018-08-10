package dedkovv

class Game(val lastFrameInd: Int = 9) {
  type FrameInd = Int
  type Score = Int

  val minPins = 0
  val maxPins = 10
  val maxNumOfTries = 2
  val maxNumOfTriesInLastFrame = 3

  var rollInd: Int = -1
  var frameInd: FrameInd = 0
  var frameToPins = Map.empty[FrameInd, Seq[Score]]
  var isFinished = false

  def roll(pins: Int): Unit = {
    require(pins >= minPins && pins <= maxPins)

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

  def score(): Int = {
    assert(isFinished)

    var scoreN = 0
    var rollInd = 0

    val rolls = frameToPins.toSeq.sortBy(_._1).flatMap(_._2)

    def isSpare(tryInd: Int): Boolean = rolls(tryInd) + rolls(tryInd + 1) == maxPins
    def isStrike(tryInd: Int): Boolean = rolls(tryInd) == maxPins

    Range(0, lastFrameInd + 1).foreach { _ =>
      if (isStrike(rollInd)) {
        scoreN += (rolls(rollInd + 1) + rolls(rollInd + 2) + maxPins)
        rollInd += 1
      } else if (isSpare(rollInd)) {
        scoreN += (rolls(rollInd + 2) + maxPins)
        rollInd += 2
      } else {
        scoreN += (rolls(rollInd) + rolls(rollInd + 1))
        rollInd += 2
      }
    }

    scoreN
  }

  private def updateFrameToPins(pins: Int): Unit = {
    frameToPins += (frameInd -> List(pins))
  }
}