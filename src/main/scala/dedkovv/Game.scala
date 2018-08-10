package dedkovv

class Game(val lastFrameInd: Int = 9) {
  private type FrameInd = Int
  private type Score = Int

  private val minPins = 0
  private val maxPins = 10
  private val maxNumOfTries = 2
  private val maxNumOfTriesInLastFrame = 3

  private var rollInd: Int = -1
  private var frameInd: FrameInd = 0
  private var frameToScores = Map.empty[FrameInd, Seq[Score]]
  private var finished = false

  def roll(pins: Int): Unit = {
    require(pins >= minPins && pins <= maxPins)

    assert(!finished)

    rollInd += 1

    if (frameToScores.contains(frameInd)) {
      val scoresPerFrameSeq = frameToScores(frameInd)

      assert(scoresPerFrameSeq.nonEmpty && (scoresPerFrameSeq.size <= maxNumOfTries))

      val scoresPerFrame = scoresPerFrameSeq.sum

      if (frameInd == lastFrameInd) {
        frameToScores += (frameInd -> (scoresPerFrameSeq :+ pins))
      } else if (scoresPerFrame == maxPins || scoresPerFrameSeq.size == maxNumOfTries) {
        frameInd += 1
        updateScores(pins)
      } else {
        val rem = maxPins - scoresPerFrameSeq.last

        assert(pins <= rem)

        frameToScores += (frameInd -> (scoresPerFrameSeq :+ pins))
      }
    } else {
      updateScores(pins)
    }

    if (frameInd == lastFrameInd) {
      val scoresPerFrameSeq = frameToScores(frameInd)
      if (scoresPerFrameSeq.size == maxNumOfTriesInLastFrame ||
        scoresPerFrameSeq.size == 2 && scoresPerFrameSeq.sum < maxPins) {
        finished = true
      }
    }
  }

  def score(): Int = {
    assert(finished)

    var scoreN = 0
    var rollInd = 0

    val rolls = frameToScores.toSeq.sortBy(_._1).flatMap(_._2)

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

  def getRollInd: Int = rollInd
  def getFrameInd: Int = frameInd
  def isFinished: Boolean = finished

  private[dedkovv] def getFrameToPins: Map[FrameInd, Seq[Score]] = frameToScores

  private def updateScores(pins: Int): Unit = {
    frameToScores += (frameInd -> List(pins))
  }
}