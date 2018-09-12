package org.babysitter

trait Babysitter {

  val START_CUTOFF = 17
  val BEDTIME = 21
  val MIDNIGHT = 24
  val END_CUTOFF = 4

  def setEndToMidnight(start: Int, startTime: Int, end: Int): Int = {
    if (startIsAfterEnd(start, end) && timeIsNotEqualOrAfterBedtime(end) && startNotEqualToEnd(startTime, end)) {
      MIDNIGHT
    } else {
      end
    }
  }

  def setStartToBedtime(startTime: Int): Int = {
    if (BEDTIME > startTime) {
      BEDTIME
    } else {
      startTime
    }
  }

  def setEndToEqualOrAfterBedtime(endTime: Int): Int = {
    if (timeIsEqualOrAfterBedtime(endTime)) {
      BEDTIME
    } else {
      endTime
    }
  }

  def setStartTime(startTime: Int): Int = {
    if (START_CUTOFF < startTime) {
      startTime
    } else {
      START_CUTOFF
    }
  }

  def setStartToStartCutoff(startTime: Int): Int = {
    if (START_CUTOFF > startTime) {
      START_CUTOFF
    } else {
      startTime
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
    if (timeIsEqualOrAfterEndCutoff(end)) {
      END_CUTOFF
    } else if (timeIsEqualOrAfterMidnight(end) && timeIsEqualOrBeforeEndCutoff(end)) {
      val hoursBeforeEnd = Math.abs(END_CUTOFF - end)
      val hoursWorked = END_CUTOFF - hoursBeforeEnd
      hoursWorked
    } else {
      0
    }
  }

  def startNotEqualToEnd(start: Int, end: Int): Boolean = {
    start != end
  }

  def startIsAfterEnd(start: Int, end: Int): Boolean = {
    start > end
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

  def timeIsNotEqualOrAfterBedtime(time: Int): Boolean = {
    !(time >= BEDTIME)
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

  def timeIsBetweenMidnightAndEndCutoff(time: Int): Boolean = {
    timeIsMidnight(time) || timeIsEqualOrBeforeEndCutoff(time)
  }

  def timeIsEqualOrAfterMidnight(time: Int): Boolean = {
    time >= 1
  }

  def payFromStartToBedtime(startTime: Int, endTime: Int, startToBedtimePay: Int = 12): Int = {
    val start: Int = setStartToStartCutoff(startTime)
    val end: Int = setEndToEqualOrAfterBedtime(endTime)

    if (timeIsEqualOrAfterStartCutoff(start) && timeIsEqualOrAfterStartCutoff(end)) {
      val workedHours = start - end
      calculatePay(startToBedtimePay, workedHours)
    } else if (timeIsEqualOrAfterStartCutoff(start) && timeIsEqualOrAfterEndCutoff(end)) {
      val workedHours = if (timeIsEqualOrAfterBedtime(start)) {
        0
      } else if (end < START_CUTOFF) {
        END_CUTOFF
      } else {
        start - (end + START_CUTOFF)
      }
      calculatePay(startToBedtimePay, workedHours)
    } else if (timeIsEqualOrAfterStartCutoff(start) && timeIsEqualOrBeforeEndCutoff(end)) {
      val workedHours = BEDTIME - start
      calculatePay(startToBedtimePay, workedHours)
    } else {
      0
    }
  }

  def payFromBedtimeToMidnight(startTime: Int, endTime: Int, payRate: Int = 8): Int = {
    val start = setStartToBedtime(startTime)
    val end = setEndToMidnight(start, startTime, endTime)

    if (timeIsOnlyBeforeBedtime(start) && (timeIsEqualOrBeforeEndCutoff(end) || timeIsMidnight(end))) {
      val hoursBetweenBedtimeToMidnight: Int = 3
      calculatePay(payRate, hoursBetweenBedtimeToMidnight)
    } else if (timeIsEqualOrBeforeBedtime(start) && timeIsEqualOrBeforeBedtime(end) && !timeIsEqualOrBeforeEndCutoff(end)){
      0
    } else if (timeIsEqualOrBeforeBedtime(start) && (timeIsEqualOrAfterBedtime(end) || timeIsEqualOrBeforeEndCutoff(end))) {
      val hoursWorked = if (timeIsEqualOrBeforeBedtime(start)) {
        BEDTIME - end
      } else {
        MIDNIGHT - start
      }
      calculatePay(payRate, hoursWorked)
    } else if (timeIsEqualOrAfterBedtime(start) && timeIsEqualOrBeforeMidnight(end)) {
      val hoursWorked = start - end
      calculatePay(payRate, hoursWorked)
    } else {
      0
    }
  }

  def startOnlyBeforeStartCutoff(start: Int): Boolean = {
    start <= 16
  }

  def noPay(): Boolean = {
    false
  }

  def payFromMidnightToEnd(startTime: Int, end: Int, isPayTotal: Boolean = false, payRate: Int = 16): Int = {
    val start: Int = setStartTime(startTime)
    val totalPayToEnd: Int = 64

    if (timeIsEqualOrAfterMidnight(start) && timeIsEqualOrBeforeEndCutoff(start) && timeIsEqualOrBeforeEndCutoff(end)) {
      val hoursWorked = hoursWorkedBetweenMidnightAndEnd(end)
      calculatePay(payRate, hoursWorked)
    } else if (timeIsBetweenMidnightAndEndCutoff(start) && timeIsEqualOrAfterEndCutoff(end)) {
      val hoursWorked = hoursWorkedBetweenMidnightAndEnd(end)
      calculatePay(payRate, hoursWorked)
    } else if (timeIsEqualOrAfterEndCutoff(start) && timeIsEqualOrBeforeMidnight(start) && timeIsEqualOrBeforeEndCutoff(end)) {
      calculatePay(payRate, end)
    } else if (timeIsEqualOrBeforeMidnight(start) || (timeIsEqualOrAfterMidnight(start) && timeIsEqualOrBeforeEndCutoff(start))) {
       val midnightToEndPay: Int = if (startOnlyBeforeStartCutoff(start)) totalPayToEnd else 0
       val total: Int = if (midnightToEndPay == 0 && isPayTotal) totalPayToEnd else 0
       total
    } else {
      totalPayToEnd
    }
  }

  def sumOfPay(start: Int, end: Int): Int = {
    val payToBedtime = payFromStartToBedtime(start, end)

    val payToMidnight = payFromBedtimeToMidnight(start, end)

    val startToMidnightTotal: Int = 72
    val hasTotalPay = if ((payToBedtime + payToMidnight) == startToMidnightTotal) true else false
    val payToEnd = payFromMidnightToEnd(start, end, hasTotalPay)

    payToBedtime + payToMidnight + payToEnd
  }

}
