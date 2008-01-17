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
      setStackTrace(t, t, origin.getClass.getName.split("\\.").last)
      throw t
    }
    def rethrowFrom(origin: Object, other: Throwable) = {
      setStackTrace(other, t, origin.getClass.getName.split("\\.").last)
      throw other
    }
    def setStackTrace(other: Throwable, t: Throwable, name: String) = other.setStackTrace((t.getStackTrace.toList.drop(1).dropWhile {x: StackTraceElement => x.toString.matches(".*" + name + ".*") }).toArray) 
  }
}
