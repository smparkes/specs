package scala.specs.matcher;
import scala.specs.matcher.Matcher._

trait NumericMatchers {
  def beStrictlyLessThan[S <% Double](n: S) = make[S]((x: S) => (x < n, x + " is strictly less than " + n, x + " is not strictly less than " + n))
  def beStrictlyGreaterThan[S <% Double](n: S) = make[S]((x: S) => (x > n, x + " is strictly greater than " + n, x + " is not strictly greater than " + n))
  def beLessThan[S <% Double](n: S) = make[S]((x: S) => (x <= n, x + " is less than " + n, x + " is not less than " + n))
  def beGreaterThan[S <% Double](n: S) = make[S]((x: S) => (x >= n, x + " is greater than " + n, x + " is not greater than " + n))
  def beClose[S <% Double](n: S, delta: S) = make[S]((x: S) => ((n - delta <= x) && (x <= n + delta), x + " is close " + n + " +/- " + delta, x + " is not close " + n + " +/- " + delta))
}
