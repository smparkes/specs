package org.specs.runner;

import _root_.junit.framework._;
import org.junit.runner.Description;
import org.junit.runner.Description._;
import org.junit.runner.manipulation._;
import org.junit.runner.notification._;

/**
 * This class is a direct copy of the JUnit38ClassRunner class which only accepts a TestCase class.<br>
 * It is used to run a JUnit3 TestSuite with the JUnit 4 library 
 */
class JUnit38SuiteRunner(klass: Class) extends org.junit.runner.Runner with Filterable with Sortable {
  
  /**
   * Aggregated test representing the whole test suite
   */
  var fTest: Test = new TestSuite(klass.asSubclass(classOf[TestSuite]))
  
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
	  createTestDescription(tc.getClass(), tc.getName());
	} else if (test.isInstanceOf[TestSuite]) {
	  val ts = test.asInstanceOf[TestSuite];
	  val description= createSuiteDescription(classOf[TestSuite]);
      for (i <- 0 to ts.testCount()-1)
		description.addChild(makeDescription(ts.testAt(i)));
	  description;
	 } else
	    // This is the best we can do in this case
		createSuiteDescription(test.getClass());
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
 * This class listens for JUnit3 results (as a TestListener) and notifies a JUnit4 RunNotifier 
 */
class OldTestClassAdaptingListener(notifier: RunNotifier)  extends TestListener {

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
      case skipped: SkippedAssertionError => notifier.fireTestIgnored(asDescription(test, skipped))
      case _ => addError(test, t)
    }
  }

  /**
   * Describe a test
   */
  def asDescription(test: Test) = createTestDescription(test.getClass(), getName(test))

  /**
   * Describe a skipped test
   */
  def asDescription(test: Test, skipped: SkippedAssertionError) = createTestDescription(test.getClass(), getName(test) + " (" + skipped.getMessage + ")")

  /**
   * Get the name of a test
   */
  def getName(test: Test) = {
	if (test.isInstanceOf[TestCase])
	  test.asInstanceOf[TestCase].getName()
	else
	  test.toString()
  }

}

