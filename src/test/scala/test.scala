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
    timeIsEqualOrAfterStartCutoff(18) should be (true)
  }

  it should "return true if time is equal to cutoff" in {
    val timeAfterCutoff: Int = 18
    timeIsEqualOrAfterStartCutoff(18) should be (true)
  }

  it should "return false if time is before cutoff" in {
    timeIsEqualOrAfterStartCutoff(16) should be (false)
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

}
