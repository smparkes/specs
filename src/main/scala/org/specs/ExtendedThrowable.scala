package org.specs

object ExtendedThrowable {
  implicit def toExtendedThrowable(t: Throwable) = new ExtendedThrowable(t)
  class ExtendedThrowable(t: Throwable) {
    def location: String = (t.getStackTrace()(0).getFileName + ":" + t.getStackTrace()(0).getLineNumber)
    
    /**
     * Throws an exception removing the traces of the object wanting to throw this exception
     * @param origin object which has be called to throw the <code>Exception</code> 
     * @param exception the exception to throw 
     */
    def rethrowFrom(origin: Object) = {
      t.setStackTrace((t.getStackTrace.toList.drop(2).dropWhile {x: StackTraceElement => origin.getClass.getName.split("\\.").last.matches(x.toString)}).toArray)
      throw t
    }
    def rethrowFrom(origin: Object, other: Throwable) = {
      other.setStackTrace((t.getStackTrace.toList.drop(2).dropWhile {x: StackTraceElement => origin.getClass.getName.split("\\.").last.matches(x.toString)}).toArray)
      throw other
    }
  }
}
