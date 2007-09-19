package scala.specs.mock
import scala.specs._
import scala.specs.Sugar._
import scala.specs.matcher._
import scala.specs.runner._

object mockParametersSuite extends JUnit3(mockParameters) 
object mockParameters extends MatchersSpecification with MovieGuardMock {
  "Mock parameters" should { usingBefore { () => {clearExample} }
    "provide a recordAndReturn method allowing to specify a stubbed return value: def mockedMethod = recordAndReturn(true)" in {
      alwaysOkGuard
      expect(atLeastOneOf) {
        mock.okForAge(20, Movie(18)); 
      }
      // by pass the rater everytime!
      alwaysOkGuard.canWatch(Watcher(20), Movie(18)) mustBe true
      alwaysOkGuard.canWatch(Watcher(16), Movie(18)) mustBe true
    }
    "provide a recordAndReturn method allowing to specify a stubbed returned function: def mockedMethod = recordAndReturn(f)" in {
      val inversedGuard = guardWith((a: Int, m: Movie) => !(new MovieRater().okForAge(a, m)))
      expect(atLeastOneOf) {
        mock.okForAge(20, Movie(18)); 
      }
      // reverse the rater!
      inversedGuard.canWatch(Watcher(20), Movie(18)) mustBe false
      inversedGuard.canWatch(Watcher(16), Movie(18)) mustBe true
    }
    "provide expectations for the passed parameters def mockedMethod = recordAndReturn(f)" in {
      val checkedGuard = guardWith((a: Int, m: Movie) => {a must beGreaterThan(0); true})
      expect(atLeastOneOf) {
        mock.okForAge(20, Movie(18)); 
      }
      // don't send negative numbers to the rater!
      assertion(checkedGuard.canWatch(Watcher(-10), Movie(18))) must failWith("-10.0 is not greater than 0")
      checkedGuard.canWatch(Watcher(10), Movie(18)) mustBe true
    }
    "provide expectations for the passed parameters def mockedMethod = record(f)" in {
      val checkedGuard = guardWithMockedRegister((m: Movie) => {m.minAge must beGreaterThan(0)})
      expect(atLeastOneOf) {
        mock.register(Movie(18)); 
      }
      // don't register negative movies numbers to the rater!
      assertion(checkedGuard.guard(Movie(-18))) must failWith("-18.0 is not greater than 0")
      checkedGuard.guard(Movie(18)) // must not fail
    }
  }
}

trait MovieGuardMock extends MovieGuardAndRater with Mocker {
  var mock: MovieRater = null 
  lazy val alwaysOkGuard = { 
    mock = new MovieRater {
      override def okForAge(a: Int, m: Movie) =  recordAndReturn(true)
      override def register(m: Movie) =  record
    }
    MovieGuard(mock)
  }
  def guardWith(f: (Int, Movie) => Boolean) = { 
    mock = new MovieRater {
      override def okForAge(a: Int, m: Movie) =  recordAndReturn(f(a, m))
    }
    MovieGuard(mock)
  }
  def guardWithMockedRegister(f: Movie => Unit) = { 
    mock = new MovieRater {
      override def register(m: Movie) =  record(f(m))
    }
    MovieGuard(mock)
  }
}
trait MovieGuardAndRater {
  case class MovieGuard(rater: MovieRater) {
    def init() = {}
    def canWatch(w: Watcher, m: Movie) = rater.okForAge(w.age, m)
    def guard(m: Movie) = rater.register(m)
  }
  case class MovieRater {
    def okForAge(a: Int, m: Movie) = a >= m.minAge
    def register(m: Movie) = {}
  }
  case class Watcher(age: Int)
  case class Movie(minAge: Int)
}

