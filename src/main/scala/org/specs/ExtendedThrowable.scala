package org.specs

object ExtendedThrowable {
  implicit def toExtendedThrowable(t: Throwable) = new ExtendedThrowable(t)
  class ExtendedThrowable(t: Throwable) {
    def location: String = (t.getStackTrace()(0).getFileName + ":" + t.getStackTrace()(0).getLineNumber) 
  }
}
