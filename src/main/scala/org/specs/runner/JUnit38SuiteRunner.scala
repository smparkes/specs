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
import org.junit.runner.Runner;
import org.junit.runner.manipulation.Filter;
import org.junit.runner.manipulation.Filterable;
import org.junit.runner.manipulation.NoTestsRemainException;
import org.junit.runner.manipulation.Sortable;
import org.junit.runner.manipulation.Sorter;
import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunNotifier;

class JUnit38SuiteRunner(klass: Class) extends Runner with Filterable with Sortable {
	var fTest: Test = new TestSuite(klass.asSubclass(classOf[TestSuite]))
	
  
  class OldTestClassAdaptingListener(fNotifier: RunNotifier)  extends TestListener {
		def endTest(test: Test) = {
			fNotifier.fireTestFinished(asDescription(test));
		}

		def startTest(test: Test) = {
			fNotifier.fireTestStarted(asDescription(test));
		}

		// Implement junit.framework.TestListener
		def addError(test: Test, t: Throwable) = {
			val failure= new Failure(asDescription(test), t);
			fNotifier.fireTestFailure(failure);
		}

		def asDescription(test: Test): Description = {
			return Description.createTestDescription(test.getClass(), getName(test));
		}

		def getName(test: Test) = {
			if (test.isInstanceOf[TestCase])
				(test.asInstanceOf[TestCase]).getName();
			else
				test.toString();
		}

		def addFailure(test: Test, t: AssertionFailedError) = {
			addError(test, t);
		}
	}


	@Override
	def run(notifier: RunNotifier) = {
		val result= new TestResult();
		result.addListener(createAdaptingListener(notifier));
		fTest.run(result);
	}

	def createAdaptingListener(notifier: RunNotifier): TestListener = {
		return new OldTestClassAdaptingListener(notifier);
	}
	
	@Override
	def getDescription(): Description = {
		return makeDescription(fTest);
	}

	def makeDescription(test: Test): Description = {
		if (test.isInstanceOf[TestCase]) {
			val tc= test.asInstanceOf[TestCase];
			return Description.createTestDescription(tc.getClass(), tc.getName());
		} else if (test.isInstanceOf[TestSuite]) {
			val ts= test.asInstanceOf[TestSuite];
			val name = if (ts.getName == null)  "" else ts.getName;
			val description= createSuiteDescription(classOf[TestSuite]);
		    
			for (i <- 0 to ts.testCount()-1)
				description.addChild(makeDescription(ts.testAt(i)));
			return description;
		} else {
			// This is the best we can do in this case
			Description.createSuiteDescription(test.getClass());
		}
	}

	def filter(filter: Filter) = {
		if (fTest.isInstanceOf[JUnit4TestAdapter]) {
			val adapter = fTest.asInstanceOf[JUnit4TestAdapter];
			adapter.filter(filter);
		}
	}

	def sort(sorter: Sorter) = {
		if (fTest.isInstanceOf[JUnit4TestAdapter]) {
			val adapter= fTest.asInstanceOf[JUnit4TestAdapter];
			adapter.sort(sorter);
		}
	}
}
