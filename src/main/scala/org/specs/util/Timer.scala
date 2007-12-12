package org.specs.util
import java.util.Calendar

trait HmsTimer extends Timer {
  var elapsed = 0L
  var millis = Calendar.getInstance.getTime.getTime
  def stop = { 
    elapsed = Calendar.getInstance.getTime.getTime - millis
    preciseTime
  }
    
  def hourMinutesSecondsMillis = {
    var totalMillis = elapsed
    val hours = totalMillis / 1000 / 3600
    totalMillis -= hours * 3600 * 1000
    val minutes = totalMillis / 1000 / 60
    totalMillis -= minutes * 60 * 1000
    val seconds = totalMillis / 1000
    val millis = totalMillis - seconds * 1000
    (hours, minutes, seconds, millis)
  }
  
  def hms = {
    val (hours, minutes, seconds, millis) = hourMinutesSecondsMillis
    def plural(v: long) = if (v > 1) "s" else ""
    var result = ""
    if (hours > 0) { result += hours + " hour" + plural(hours) + " " } 
    if (minutes > 0) { result += minutes + " minute" + plural(minutes) + " " } 
    result += (seconds + " second" + plural(seconds))
    result
  }
  
  def preciseTime = {
    val (hours, minutes, seconds, millis) = hourMinutesSecondsMillis
    hms + ", " + millis + " ms"
  }
}

trait Timer {
  def stop: String
  def hms: String
}

class SimpleTimer extends HmsTimer

