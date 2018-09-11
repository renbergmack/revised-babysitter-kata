package org.babysitter

import org.scalatest._
import Matchers._

class BabysitterTest extends FlatSpec with BabysitterTools {

  "setStartTime" should "return 17 when an earlier start time is declared" in {
    val earlierStart: Int = 16
    setStartTime(earlierStart) should be (17)
  }

  it should "return a the time when a time later than 17 is declared" in {
    val laterStart: Int = 18
    setStartTime(laterStart) should be (18)
  }

  "setEndTime" should "return 4 when a time later than 4 is declared" in {
    val lateEnd: Int = 5
    setEndTime(lateEnd) should be (4)
  }

  it should "return the end time when a time earlier than 4 is declared" in {
    val lateEnd: Int = 3
    setEndTime(lateEnd) should be (3)
  }

  "roundToNextHour" should "return next hourly time when minutes less than 30" in {
    val earlyStart: Double = 17.4
    roundToNextHour(earlyStart) should be (18)
  }

  it should "return next hourly time when minutes more than 30" in {
    val laterStart: Double = 18.6
    roundToNextHour(laterStart) should be (19)
  }

  "timeIsEqualOrAfterStartCutoff" should "return true if time is after start cutoff" in {
    val timeAfterCutoff: Int = 18
    timeIsEqualOrAfterStartCutoff(timeAfterCutoff) should be (true)
  }

  it should "return true if time is equal to start cutoff" in {
    val timeEqualToCutoff: Int = 17
    timeIsEqualOrAfterStartCutoff(timeEqualToCutoff) should be (true)
  }

  it should "return false if time is before start cutoff" in {
    val timeBeforeCutoff: Int = 16
    timeIsEqualOrAfterStartCutoff(timeBeforeCutoff) should be (false)
  }

  "timeIsEqualOrAfterEndCutoff" should "return true if time is after end cutoff" in {
    val timeAfterCutoff: Int = 5
    timeIsEqualOrAfterEndCutoff(timeAfterCutoff) should be (true)
  }

  it should "return true if time is equal to end cutoff" in {
    val timeEqualToCutoff: Int = 4
    timeIsEqualOrAfterEndCutoff(timeEqualToCutoff) should be (true)
  }

  it should "return false if time is before end cutoff" in {
    val timeBeforeCutoff: Int = 3
    timeIsEqualOrAfterStartCutoff(timeBeforeCutoff) should be (false)
  }

  "timeIsEqualOrBeforeEndCutoff" should "return true if time is before end cutoff" in {
    val timeBeforeCutoff: Int = 3
    timeIsEqualOrBeforeEndCutoff(timeBeforeCutoff) should be (true)
  }

  it should "return true if time is equal to end cutoff" in {
    val timeEqualToCutoff: Int = 4
    timeIsEqualOrAfterEndCutoff(timeEqualToCutoff) should be (true)
  }

  it should "return false if time is after end cutoff" in {
    val timeBeforeCutoff: Int = 5
    timeIsEqualOrAfterStartCutoff(timeBeforeCutoff) should be (false)
  }

  "timeIsEqualOrBeforeMidnight" should "return true if time is before midnight" in {
    val timeBeforeMidnight: Int = 23
    timeIsEqualOrBeforeMidnight(timeBeforeMidnight) should be (true)
  }

  it should "return true if time is equal to midnight" in {
    val timeEqualToMidnight: Int = 24
    timeIsEqualOrBeforeMidnight(timeEqualToMidnight) should be (true)
  }

  it should "return false if time is after midnight" in {
    val timeAfterMidnight: Int = 25
    timeIsEqualOrBeforeMidnight(timeAfterMidnight) should be (false)
  }

  "timeIsEqualOrAfterBedtime" should "return true if time is after bedtime" in {
    val timeAfterBedtime: Int = 22
    timeIsEqualOrAfterBedtime(timeAfterBedtime) should be (true)
  }

  it should "return true if time is equal to end bedtime" in {
    val timeEqualToBedtime: Int = 21
    timeIsEqualOrAfterBedtime(timeEqualToBedtime) should be (true)
  }

  it should "return false if time is before end bedtime" in {
    val timeBeforeBedtime: Int = 19
    timeIsEqualOrAfterBedtime(timeBeforeBedtime) should be (false)
  }

  "timeIsEqualOrBeforeBedtime" should "return true if time is before bedtime" in {
    val timeBeforeBedtime: Int = 19
    timeIsEqualOrBeforeBedtime(timeBeforeBedtime) should be (true)
  }

  it should "return true if time is equal to bedtime" in {
    val timeEqualToBedtime: Int = 21
    timeIsEqualOrBeforeBedtime(timeEqualToBedtime) should be (true)
  }

  it should "return false if time is after bedtime" in {
    val timeAfterBedtime: Int = 22
    timeIsEqualOrBeforeBedtime(timeAfterBedtime) should be (false)
  }

  "timeIsOnlyBeforeBedtime" should "return true if time is before bedtime" in {
    val timeBeforeBedtime: Int = 19
    timeIsOnlyBeforeBedtime(timeBeforeBedtime) should be (true)
  }

  it should "return true if time is after bedtime" in {
    val timeAfterBedtime: Int = 22
    timeIsOnlyBeforeBedtime(timeAfterBedtime) should be (false)
  }

  "timeIsMidnight" should "return true if is midnight" in {
    val mignight: Int = 24
    timeIsMidnight(mignight) should be (true)
  }

  it should "return true if is NOT midnight" in {
    val notMignight: Int = 23
    timeIsMidnight(notMignight) should be (false)
  }

  "timeIsEqualOrAfterMidnight" should "return true if is after midnight" in {
    val afterMignight: Int = 25
    timeIsEqualOrAfterMidnight(afterMignight) should be (true)
  }

  it should "return true if is equal to midnight" in {
    val mignight: Int = 24
    timeIsEqualOrAfterMidnight(mignight) should be (true)
  }

  it should "return false if is before to midnight" in {
    val beforeMidnight: Int = 0
    timeIsEqualOrAfterMidnight(beforeMidnight) should be (false)
  }

  "timeIsBetweenMidnightAndEndCutoff" should "return true if start is after midnight or before end cutoff" in {
    val start: Int = 1
    timeIsBetweenMidnightAndEndCutoff(start) should be (true)
  }

  it should "return false if start is after end cutoff" in {
    val start: Int = 5
    timeIsBetweenMidnightAndEndCutoff(start) should be (false)
  }

  it should "return false if start is before midnight" in {
    val start: Int = 23
    timeIsBetweenMidnightAndEndCutoff(start) should be (false)
  }

  "calculatePay" should "return payment amount if time and pay rate are positive" in {
    val hours: Int = 3
    val payRate: Int = 8
    calculatePay(hours, payRate) should be (24)
  }

  it should "return payment amount if time or pay rate are negative" in {
    val hours: Int = -3
    val payRate: Int = 8
    calculatePay(hours, payRate) should be (24)
  }

  "hoursWorkedBetweenMidnightAndEnd" should "return correct hours worked at end cutoff" in {
    val end: Int = 4
    hoursWorkedBetweenMidnightAndEnd(end) should be (4)
  }

  it should "return correct hours worked if time is before end cutoff" in {
    val end: Int = 2
    hoursWorkedBetweenMidnightAndEnd(end) should be (2)
  }

  "payFromStartToBedtime" should "return no pay if start is past bedtime(21)" in {
    val start = 21
    val end = 4
    payFromStartToBedtime(start, end) should be (0)
  }

  it should "return 12 dollar pay if start is before bedtime" in {
    val start = 17
    val end = 21
    payFromStartToBedtime(start, end) should be (48)
  }

  it should "return 12 dollar pay if end is after bedtime" in {
    val start = 17
    val end = 4
    payFromStartToBedtime(start, end) should be (48)
  }

  it should "return 12 dollar pay if end is before bedtime" in {
    val start = 17
    val end = 20
    payFromStartToBedtime(start, end) should be (36)
  }

  it should "return 12 dollar pay if start is before bedtime and end is after midnight" in {
    val start = 18
    val end = 2
    payFromStartToBedtime(start, end) should be (36)
  }

  "payFromBedtimeToMidnight" should "return no pay if start is past midnight(24)" in {
    val start = 24
    val end = 24
    payFromBedtimeToMidnight(start, end) should be (0)
  }

  it should "return 8 dollar pay if start is at bedtime" in {
    val start = 21
    val end = 24
    payFromBedtimeToMidnight(start, end) should be (24)
  }

  it should "return 8 dollar pay if end is after midnight" in {
    val start = 21
    val end = 4
    payFromBedtimeToMidnight(start, end) should be (24)
  }

  it should "return 8 dollar pay if start and end is between bedtime and midnight" in {
    val start = 22
    val end = 23
    payFromBedtimeToMidnight(start, end) should be (8)
  }

  it should "return pay when start is before bedtime and end is after midnight" in {
    val start = 17
    val end = 4
    payFromBedtimeToMidnight(start, end) should be (24)
  }

  "payFromMidnightToEnd" should "return pay if start is past end(4)" in {
    val start = 5
    val end = 4
    payFromMidnightToEnd(start, end) should be (64)
  }

  it should "return 16 dollar pay if start is at midnight" in {
    val start = 24
    val end = 4
    payFromMidnightToEnd(start, end) should be (64)
  }

  it should "return 16 dollar pay if end is after midnight" in {
    val start = 24
    val end = 5
    payFromMidnightToEnd(start, end) should be (64)
  }

}
