package org.specs.runner;

import _root_.junit.framework._;
import org.junit.runner.Description;
import org.junit.runner.Description._;
import org.junit.runner.manipulation._;
import org.junit.runner.notification._;
import org.specs.collection.JavaCollectionsConversion._
/**
 * This class is a direct copy of the JUnit38ClassRunner class which only accepts a TestCase class.<br>
 * It is used to run a JUnit3 TestSuite with the JUnit 4 library 
 */
class JUnit38SuiteRunner(klass: java.lang.Class[T] forSome {type T <: Test}) extends org.junit.runner.Runner with Filterable with Sortable with TestDescription {
  
  /**
   * Aggregated test representing the whole test suite
   */
  var fTest: Test = klass.newInstance
  
  /**
   * Runs the test suite by passing a JUnit4 RunNotifier which is wrapped in a JUnit3 TestListener to be able to run JUnit3 tests
   */
  override def run(notifier: RunNotifier) = {
	val result= new TestResult();
	result.addListener(createAdaptingListener(notifier));
	fTest.run(result);
  }

  /**
   * Adapt a JUnit4 RunNotifier with a JUnit3 TestListener
   */
  def createAdaptingListener(notifier: RunNotifier) = new OldTestClassAdaptingListener(notifier)

  /**
   * Adapt a JUnit4 RunNotifier with a JUnit3 TestListener
   */
  override def getDescription(): Description = makeDescription(fTest)

  /**
   * Create a Description from a TestCase or a TestSuite object
   */
  def makeDescription(test: Test): Description = {
	if (test.isInstanceOf[TestCase]) {
	  val tc = test.asInstanceOf[TestCase];
	  asDescription(tc)
	}
	else if (test.isInstanceOf[JUnitSuite]) {
  	   val ts = test.asInstanceOf[JUnitSuite];
       var name = ts.getName;
       if (name == null) name = ""
       val description= createSuiteDescription(name, null);
       for (suite <- ts.suites) {
		 description.addChild(makeDescription(suite))
       }
       for (t <- ts.testCases) {
         description.addChild(makeDescription(t))
       }
	   description;
	 } 
     else {
	   // This is the best we can do in this case
	   createSuiteDescription(test.getClass());
     }
  }

  /**
   * Nothing to filter
   */
  def filter(filter: Filter) = {}
  
  /**
   * Nothing to sort
   */
  def sort(sorter: Sorter) = {}
}
/**
 * Common methods to create descriptions from a test
 */
trait TestDescription {
  /**
   * Describe a test including its hashCode instead of its class name. If the class name is included, some tests may
   * not render properly as there can only be one test with a given in a given class.
   * For specs it is different as there can be the same name in 2 different suts (and those suts will be represented by the
   * same JUnit class: ExampleTestCase).
   * 
   * This uses the createSuiteDescription method from JUnit as it is the only way to create a Description object having
   * the required name.
   */
  def asDescription(test: Test) = createSuiteDescription(getName(test) +"("+test.hashCode.toString+")", null)


  /**
   * Get the name of a test
   * @return the name of the TestCase or toString if it is just the <code>Test</code> interface (maybe a TestSuite)
   */
  def getName(test: Test) = {
	  if (test.isInstanceOf[TestCase])
         test.asInstanceOf[TestCase].getName
      else
         test.toString
  }

}
/**
 * This class listens for JUnit3 results (as a TestListener) and notifies a JUnit4 RunNotifier 
 */
class OldTestClassAdaptingListener(notifier: RunNotifier)  extends TestListener with TestDescription {

  /**
   * Notifies the notifier of a test start 
   */
  def startTest(test: Test) = notifier.fireTestStarted(asDescription(test))

  /**
   * Notifies the notifier of a test finish 
   */
  def endTest(test: Test) = notifier.fireTestFinished(asDescription(test))

  /**
   * Notifies the notifier of a test failure (an Error in JUnit3 is a Failure in JUnit4) 
   */
  def addError(test: Test, t: Throwable) = notifier.fireTestFailure(new Failure(asDescription(test), t))
	
  /**
   * Notifies the notifier of a test failure.
   * specs specificity: if the failure is a SkippedAssertionError, then notify of a skip
   */
  def addFailure(test: Test, t: AssertionFailedError) = {
    t match {
      // unfortunately the skip message can not be included for display
      case skipped: SkippedAssertionError => notifier.fireTestIgnored(asDescription(test))
      case _ => addError(test, t)
    }
  }


}

