package org.babysitter

trait BabysitterTools {

  val START_CUTOFF = 17

  def setStartTime(startTime: Int): Int = {
    if (START_CUTOFF < startTime) {
      startTime
    } else {
      START_CUTOFF
    }
  }

  def setEndTime(endTime: Int): Int = {
    if (4 > endTime) {
      endTime
    } else {
      4
    }
  }

}
