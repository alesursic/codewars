package sr.ctw;

import org.junit.runner.notification.RunNotifier;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.model.InitializationError;

public class BlockRunner extends BlockJUnit4ClassRunner {
    public BlockRunner(Class<?> klass) throws InitializationError {
        super(klass);
    }

    @Override
    public void run(RunNotifier notifier) {
        notifier.addListener(SessionMgrListener.singleton());
        super.run(notifier);
    }
}
