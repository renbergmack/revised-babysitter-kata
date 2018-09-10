package org.babysitter

trait BabysitterTools {

  def setStartTime(time: Int): Int = {
      if (17 < time) {
        time
      } else {
        17
      }
  }

}
