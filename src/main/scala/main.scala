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
    val payToBedtime: Int = hours * payRate
    Math.abs(payToBedtime)
  }

  def hoursWorkedBetweenMidnightAndEnd(end: Int): Int = {
    if (timeIsEqualOrAfterEndCutoff(end)) {
      END_CUTOFF
    } else if (timeIsEqualOrAfterMidnight(end) && timeIsEqualOrBeforeEndCutoff(end)) {
      val hoursBeforeEnd: Int = Math.abs(END_CUTOFF - end)
      val hoursWorked: Int = END_CUTOFF - hoursBeforeEnd
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

  def startOnlyBeforeStartCutoff(start: Int): Boolean = {
    start <= 16
  }

  def noPay(pay: Int): Boolean = {
    pay == 0
  }

  def payFromStartToBedtime(originalStart: Int, originalEnd: Int): Int = {
    val start: Int = setStartToStartCutoff(originalStart)
    val end: Int = setEndToEqualOrAfterBedtime(originalEnd)
    val startToBedtimePayRate: Int = 12

    if (timeIsEqualOrAfterStartCutoff(start) && timeIsEqualOrAfterStartCutoff(end)) {
      val workedHours: Int = start - end
      calculatePay(startToBedtimePayRate, workedHours)
    } else if (timeIsEqualOrAfterStartCutoff(start) && timeIsEqualOrAfterEndCutoff(end)) {
      val workedHours: Int = if (timeIsEqualOrAfterBedtime(start)) {
          0
        } else {
          END_CUTOFF
        }
      calculatePay(startToBedtimePayRate, workedHours)
    } else {
      val workedHours: Int = BEDTIME - start
      calculatePay(startToBedtimePayRate, workedHours)
    }
  }

  def payFromBedtimeToMidnight(originalStart: Int, originalEnd: Int): Int = {
    val start: Int = setStartToBedtime(originalStart)
    val end: Int = setEndToMidnight(start, originalStart, originalEnd)
    val bedtimeToMidnightPayRate: Int = 8

    if (timeIsEqualOrBeforeBedtime(start) && timeIsEqualOrBeforeBedtime(end) && !timeIsEqualOrBeforeEndCutoff(end)){
      0
    } else if (timeIsEqualOrBeforeBedtime(start) && (timeIsEqualOrAfterBedtime(end) || timeIsEqualOrBeforeEndCutoff(end))) {
      val hoursWorked: Int = BEDTIME - end
      calculatePay(bedtimeToMidnightPayRate, hoursWorked)
    } else {
      val hoursWorked: Int = start - end
      calculatePay(bedtimeToMidnightPayRate, hoursWorked)
    }
  }

  def payFromMidnightToEnd(originalStart: Int, end: Int, isPayTotal: Boolean = false): Int = {
    val start: Int = setStartTime(originalStart)
    val maxPayToEnd: Int = 64
    val midnightToEndPayRate: Int = 16

    if (timeIsBetweenMidnightAndEndCutoff(start) && timeIsEqualOrAfterEndCutoff(end)) {
      val hoursWorked: Int = hoursWorkedBetweenMidnightAndEnd(end)
      calculatePay(midnightToEndPayRate, hoursWorked)
    } else if (timeIsEqualOrAfterEndCutoff(start) && timeIsEqualOrBeforeMidnight(start) && timeIsEqualOrBeforeEndCutoff(end)) {
      calculatePay(midnightToEndPayRate, end)
    } else {
      val midnightToEndPay: Int = if (startOnlyBeforeStartCutoff(start)) maxPayToEnd else 0
      val pay: Int = if (noPay(midnightToEndPay) && isPayTotal) maxPayToEnd else 0
      pay
    }
  }

  def sumOfPay(start: Int, end: Int): Int = {
    val payToBedtime: Int = payFromStartToBedtime(start, end)
    val payToMidnight: Int = payFromBedtimeToMidnight(start, end)

    val maxStartToMidnightPay: Int = 72
    val hasTotalPay: Boolean = if ((payToBedtime + payToMidnight) == maxStartToMidnightPay) true else false
    val payToEnd: Int = payFromMidnightToEnd(start, end, hasTotalPay)

    payToBedtime + payToMidnight + payToEnd
  }
}
