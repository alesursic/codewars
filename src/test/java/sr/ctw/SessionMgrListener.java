package sr.ctw;

import org.junit.runner.Description;
import org.junit.runner.notification.RunListener;

public class SessionMgrListener extends RunListener {
    private static final SessionMgrListener singletonInstance = new SessionMgrListener();

    private volatile Class<?> firstTestClass;

    @Override
    public void testSuiteStarted(Description description) throws Exception {
        if (firstTestClass == null) {
            firstTestClass = description.getTestClass();
            System.out.println("test suite started");
        }
    }

    @Override
    public void testSuiteFinished(Description description) throws Exception {
        if (firstTestClass == description.getTestClass()) { //pointer equality
            firstTestClass = null;
            System.out.println("test suite finished");
        }
    }

    public static SessionMgrListener singleton() {
        return singletonInstance;
    }
}
