package org.specs.runner;

import _root_.junit.extensions.TestDecorator;
import _root_.junit.framework.AssertionFailedError;
import _root_.junit.framework.JUnit4TestAdapter;
import _root_.junit.framework.JUnit4TestCaseFacade;
import _root_.junit.framework.Test;
import _root_.junit.framework.TestCase;
import _root_.junit.framework.TestListener;
import _root_.junit.framework.TestResult;
import _root_.junit.framework.TestSuite;
import org.junit.runner.Description;
import org.junit.runner.Description._;
import org.junit.runner.manipulation.Filter;
import org.junit.runner.manipulation.Filterable;
import org.junit.runner.manipulation.NoTestsRemainException;
import org.junit.runner.manipulation.Sortable;
import org.junit.runner.manipulation.Sorter;
import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunNotifier;

/**
 * This class is a direct copy of the JUnit38ClassRunner class which only accepts a TestCase class.<br>
 * It is used to run JUnit3 TestSuite with the JUnit 4 library 
 */
class JUnit38SuiteRunner(klass: Class) extends org.junit.runner.Runner with Filterable with Sortable {
  var fTest: Test = new TestSuite(klass.asSubclass(classOf[TestSuite]))
  
  override def run(notifier: RunNotifier) = {
	val result= new TestResult();
	result.addListener(createAdaptingListener(notifier));
	fTest.run(result);
  }

  def createAdaptingListener(notifier: RunNotifier): TestListener = new OldTestClassAdaptingListener(notifier)

  override def getDescription(): Description = makeDescription(fTest)

  def makeDescription(test: Test): Description = {
	if (test.isInstanceOf[TestCase]) {
	  val tc = test.asInstanceOf[TestCase];
	  Description.createTestDescription(tc.getClass(), tc.getName());
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

  def filter(filter: Filter) = if (fTest.isInstanceOf[JUnit4TestAdapter]) {
	  fTest.asInstanceOf[JUnit4TestAdapter].filter(filter);
  }
  
  def sort(sorter: Sorter) = if (fTest.isInstanceOf[JUnit4TestAdapter]) {
	fTest.asInstanceOf[JUnit4TestAdapter].sort(sorter);
  }
}

class OldTestClassAdaptingListener(fNotifier: RunNotifier)  extends TestListener {
  def endTest(test: Test) = {
    fNotifier.fireTestFinished(asDescription(test))
  }

  def startTest(test: Test) = {
	fNotifier.fireTestStarted(asDescription(test));
  }

  // Implement junit.framework.TestListener
  def addError(test: Test, t: Throwable) = fNotifier.fireTestFailure(new Failure(asDescription(test), t))
	
  def asDescription(test: Test): Description = Description.createTestDescription(test.getClass(), getName(test))
  def asDescription(test: Test, skipped: SkippedAssertionError): Description = {
    Description.createTestDescription(test.getClass(), getName(test) + " (" + skipped.getMessage + ")")
  }

  def getName(test: Test) = {
	if (test.isInstanceOf[TestCase])
	  test.asInstanceOf[TestCase].getName()
	else
	  test.toString()
  }

  def addFailure(test: Test, t: AssertionFailedError) = {
    t match {
      case skipped: SkippedAssertionError => fNotifier.fireTestIgnored(asDescription(test, skipped))
      case _ => addError(test, t)
    }
  }
}

