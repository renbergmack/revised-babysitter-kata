package org.babysitter

import org.scalatest._
import Matchers._

class BabysitterTest extends FlatSpec with BabysitterTools {

  "setStartTime" should "return 17 when an earlier start time is declared" in {
    setStartTime() should be (17)
  }

  it should "return a the time when a time later than 17 is declared" in {
    setStartTime() should be (18)
  }

}
