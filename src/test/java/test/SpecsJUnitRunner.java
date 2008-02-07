package test;

import junit.framework.Test;
import junit.framework.TestCase;

public class SpecsJUnitRunner extends TestCase {
	public static Test suite() {
		final String suiteClassName = System.getProperty("specs");
		try {
			Class<?> suiteClass = Thread.currentThread().getContextClassLoader().loadClass(suiteClassName);
			Object suite = suiteClass.newInstance();
			System.out.println("class is " + suite.getClass().getName());
			return (Test) suite;
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
