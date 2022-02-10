package sr.ctw;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(BlockRunner.class)
public class TC0 {
    @BeforeClass
    public static void before() {
        System.out.println("before tc0");
    }

    @Test
    public void tm00() {
        System.out.println("tm00");
    }

    @Test
    public void tm01() {
        System.out.println("tm01");
    }
}
