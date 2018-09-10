package org.babysitter

trait BabysitterTools {

  val START_CUTOFF = 17
  val END_CUTOFF = 4

  def setStartTime(startTime: Int): Int = {
    if (START_CUTOFF < startTime) {
      startTime
    } else {
      START_CUTOFF
    }
  }

  def setEndTime(endTime: Int): Int = {
    if (END_CUTOFF > endTime) {
      endTime
    } else {
      END_CUTOFF
    }
  }

  def roundToNextHour(time: Double): Int = {
    Math.ceil(time).toInt
  }

  def payFromStartToBedtime(start: Int, end: Int): Int = {
    val payRate: Int = 12
    if (start >= START_CUTOFF && end >= START_CUTOFF) {
      val workedHours = start - end
      val payToBedtime = workedHours * payRate
      Math.abs(payToBedtime)
    } else {
      0
    }
  }

}
