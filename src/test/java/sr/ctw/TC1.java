package sr.ctw;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(BlockRunner.class)
public class TC1 {
    @BeforeClass
    public static void before() {
        System.out.println("before tc1");
    }

    @Test
    public void tm10() {
        System.out.println("tm10");
    }
}
