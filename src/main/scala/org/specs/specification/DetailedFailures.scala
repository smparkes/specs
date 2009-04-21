/**
 * Copyright (c) 2007-2009 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS INTHE SOFTWARE.
 */
package org.specs.specification

trait DetailedFailures {
  /** by default no full details are reported by specifications */
  implicit var detailedFailures: Detailed = noDetails()

  /** detailled diffs enable showing the differences when comparing the toString result of 2 objects supposed to be == */
  def detailedDiffs() = { detailedFailures = fullDetails("()") }

  /** detailled diffs enable showing the differences when comparing the toString result of 2 objects supposed to be == */
  def detailedDiffs(separators: String) = { detailedFailures = fullDetails(separators) }

  /** reset the detailled diffs to no diffs */
  def noDetailedDiffs() = { detailedFailures = noDetails() }
}
/** abstract data type representing Detailed information about failures */
abstract class Detailed
/** no details should be shown */
case class noDetails() extends Detailed
/** all details should be shown */
case class fullDetails(separators: String) extends Detailed {
  def this() = this("()") 
}
