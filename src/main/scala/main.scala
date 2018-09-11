package org.babysitter

trait BabysitterTools {

  val START_CUTOFF = 17
  val BEDTIME = 21
  val MIDNIGHT = 24
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

  def hoursWorkedBetweenMidnightAndEnd(end: Int): Int = {
    val hoursBeforeEnd = Math.abs(END_CUTOFF - end)
    val hoursWorked = end - hoursBeforeEnd
    hoursWorked
  }

  def timeIsEqualOrAfterStartCutoff(time: Int): Boolean = {
    time >= START_CUTOFF
  }

  def timeIsEqualOrBeforeEndCutoff(time: Int): Boolean = {
    time <= END_CUTOFF
  }

  def timeIsEqualOrAfterEndCutoff(time: Int): Boolean = {
    time >= END_CUTOFF
  }

  def timeIsEqualOrBeforeMidnight(time: Int): Boolean = {
    time <= MIDNIGHT
  }

  def timeIsEqualOrAfterBedtime(time: Int): Boolean = {
    time >= BEDTIME
  }

  def timeIsEqualOrBeforeBedtime(time: Int): Boolean = {
    time <= BEDTIME
  }

  def timeIsOnlyBeforeBedtime(time: Int): Boolean = {
    time < BEDTIME
  }

  def timeIsMidnight(time: Int): Boolean = {
    time == MIDNIGHT
  }

  def timeIsEqualOrAfterMidnight(time: Int): Boolean = {
    time >= 1
  }

  def payFromStartToBedtime(start: Int, end: Int, startToBedtimePay: Int = 12): Int = {
    if (timeIsEqualOrAfterStartCutoff(start) && timeIsEqualOrAfterStartCutoff(end)) {
      val workedHours = start - end
      calculatePay(startToBedtimePay, workedHours)
    } else if (timeIsEqualOrAfterStartCutoff(start) && timeIsEqualOrAfterEndCutoff(end)) {
      val workedHours = start - (end + START_CUTOFF)
      calculatePay(startToBedtimePay, workedHours)
    } else if (timeIsEqualOrAfterStartCutoff(start) && timeIsEqualOrBeforeEndCutoff(end)) {
      val workedHours = BEDTIME - start
      calculatePay(startToBedtimePay, workedHours)
    } else {
      0
    }
  }

  def payFromBedtimeToMidnight(start: Int, end: Int, payRate: Int = 8): Int = {
    if (timeIsOnlyBeforeBedtime(start) && (timeIsEqualOrBeforeEndCutoff(end) || timeIsMidnight(end))) {
      calculatePay(payRate, 3)
    } else if (timeIsEqualOrBeforeBedtime(start) && (timeIsEqualOrAfterBedtime(end) || timeIsEqualOrBeforeEndCutoff(end))) {
      val hoursWorked = start - MIDNIGHT
      calculatePay(payRate, hoursWorked)
    } else if (timeIsEqualOrAfterBedtime(start) && timeIsEqualOrBeforeMidnight(end)) {
      val hoursWorked = start - end
      calculatePay(payRate, hoursWorked)
    } else {
      0
    }
  }

  def payFromMidnightToEnd(start: Int, end: Int, payRate: Int = 16): Int = {
    if (timeIsEqualOrAfterMidnight(start) && timeIsEqualOrBeforeEndCutoff(start) && timeIsEqualOrBeforeEndCutoff(end)) {
      val hoursWorked = hoursWorkedBetweenMidnightAndEnd(end)
      calculatePay(payRate, hoursWorked)
    } else if (timeIsEqualOrAfterEndCutoff(start) && timeIsEqualOrBeforeMidnight(start) && timeIsEqualOrBeforeEndCutoff(end)) {
      calculatePay(payRate, end)
    } else {
      0
    }
  }

}
