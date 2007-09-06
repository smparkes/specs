package scala.specs.matcher;
import scala.specs.matcher.Matcher._

/**
 * The <code>NumericMatchers</code> trait provides matchers which allow numerical comparisons
 */
trait NumericMatchers {
  /**
   * Matches if x < n
   */   
  def beStrictlyLessThan[S <% Double](n: S) = make[S]((x: S) => (x < n, x + " is strictly less than " + n, x + " is not strictly less than " + n))

  /**
   * Matches if x > n
   */   
  def beStrictlyGreaterThan[S <% Double](n: S) = make[S]((x: S) => (x > n, x + " is strictly greater than " + n, x + " is not strictly greater than " + n))

  /**
   * Matches if x <= n
   */   
  def beLessThan[S <% Double](n: S) = make[S]((x: S) => (x <= n, x + " is less than " + n, x + " is not less than " + n))

  /**
   * Matches if x >= n
   */   
  def beGreaterThan[S <% Double](n: S) = make[S]((x: S) => (x >= n, x + " is greater than " + n, x + " is not greater than " + n))

  /**
   * Matches if x = n +/- delta
   */   
  def beCloseTo[S <% Double](n: S, delta: S) = make[S]((x: S) => ((n - delta <= x) && (x <= n + delta), x + " is close " + n + " +/- " + delta, x + " is not close " + n + " +/- " + delta))
}
