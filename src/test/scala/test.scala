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
    setEndTime() should be (4)
  }

}
