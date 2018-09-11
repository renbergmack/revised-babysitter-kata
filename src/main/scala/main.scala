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

  def calculatePay(payRate: Int, hours: Int): Int = {
    val payToBedtime = hours * payRate
    Math.abs(payToBedtime)
  }

  def timeIsEqualOrAfterStartCutoff(time: Int): Boolean = {
    time >= START_CUTOFF
  }

  def timeIsEqualOrAfterEndCutoff(time: Int): Boolean = {
    time >= END_CUTOFF
  }

  def payFromStartToBedtime(start: Int, end: Int, startToBedtimePay: Int = 12): Int = {
    if (timeIsEqualOrAfterStartCutoff(start) && timeIsEqualOrAfterStartCutoff(end)) {
      val workedHours = start - end
      calculatePay(startToBedtimePay, workedHours)
    } else if (timeIsEqualOrAfterStartCutoff(start) && timeIsEqualOrAfterEndCutoff(end)) {
      val workedHours = start - (end + START_CUTOFF)
      calculatePay(startToBedtimePay, workedHours)
    } else if (timeIsEqualOrAfterStartCutoff(start) && end <= END_CUTOFF) {
      val workedHours = (21 - start)
      calculatePay(startToBedtimePay, workedHours)
    } else {
      0
    }
  }

}
