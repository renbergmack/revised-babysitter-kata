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

  "payFromStartToBedtime" should "return no pay if start is past bedtime(21)" in {
    payFromStartToBedtime() should be (0)
  }

  it should "return 12 dollar pay if start is before bedtime" in {
    payFromStartToBedtime() should be (48)
  }

}
