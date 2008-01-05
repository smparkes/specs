package test;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class SpecsJUnitRunner extends TestCase {
	public static Test suite() {
		final String suiteClassName = System.getProperty("specs");
		try {
			Class<?> suiteClass = Thread.currentThread().getContextClassLoader().loadClass(suiteClassName);
			TestSuite testSuite = (TestSuite) suiteClass.newInstance();
			return (testSuite);
		} catch (InstantiationException e) {
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			e.printStackTrace();
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		}
		return null;
	} 

}
