package org.specs.collection
import scala.collection.mutable.ListBuffer

object ExtendedSeq {
  /** @return an ExtendedSeq object with more functionalities */
  implicit def seqToExtendedSeq[T](seq: Seq[T]) = new ExtendedSeq[T](seq)
  
  /**
   * See the description of the ExtendedSeq object
   */
  class ExtendedSeq[T](seq: Seq[T]) {
    /**
     * apply the function f to the seq and filters the results if the result is None
     */
    def mapFilter(f: T => Option[T]): Seq[T] = {
      seq.map(f(_)).filter(_.isDefined).map(_.get)
    } 
  }
}
