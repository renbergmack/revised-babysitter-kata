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

  def timeIsEqualOrAfterStartCutoff(time: Int): Boolean = {
    time >= START_CUTOFF
  }

  def calculatePay(payRate: Int, hours: Int): Int = {
    val payToBedtime = hours * payRate
    Math.abs(payToBedtime)
  }

  def payFromStartToBedtime(start: Int, end: Int, payRate: Int = 12): Int = {
    if (timeIsEqualOrAfterStartCutoff(start) && timeIsEqualOrAfterStartCutoff(end)) {
      val workedHours = start - end
      val payToBedtime = workedHours * payRate
      Math.abs(payToBedtime)
    } else {
      0
    }
  }

}
