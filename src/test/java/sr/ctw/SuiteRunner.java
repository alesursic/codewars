package sr.ctw;

import org.junit.runner.notification.RunNotifier;
import org.junit.runners.Suite;
import org.junit.runners.model.InitializationError;

public class SuiteRunner extends Suite {
    public SuiteRunner(Class<?> klass) throws InitializationError {
        super(
                klass,
                new Class[]{
                        TC0.class,
                        TC1.class
                }
        );
    }

    @Override
    public void run(RunNotifier notifier) {
        notifier.addListener(SessionMgrListener.singleton());
        super.run(notifier);
    }
}
