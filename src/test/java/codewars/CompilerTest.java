package codewars;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class CompilerTest {
    Compiler compiler = new Compiler();

    @Test
    public void testSimpleProg() {
        String prog = "[ x y z ] ( 2*3*x + 5*y - 3*z ) / (1 + 3 + 2*2)";

        // {'op':'/','a':{'op':'-','a':{'op':'+','a':{'op':'*','a':{'op':'*','a':{'op':'imm','n':2},'b':{'op':'imm','n':3}},'b':{'op':'arg','n':0}},'b':{'op':'*','a':{'op':'imm','n':5},'b':{'op':'arg','n':1}}},'b':{'op':'*','a':{'op':'imm','n':3},'b':{'op':'arg','n':2}}},'b':{'op':'+','a':{'op':'+','a':{'op':'imm','n':1},'b':{'op':'imm','n':3}},'b':{'op':'*','a':{'op':'imm','n':2},'b':{'op':'imm','n':2}}}}
        Ast t1 = new BinOp("/", new BinOp("-", new BinOp("+", new BinOp("*", new BinOp("*", new UnOp("imm", 2), new UnOp("imm", 3)), new UnOp("arg", 0)), new BinOp("*", new UnOp("imm", 5), new UnOp("arg", 1))), new BinOp("*", new UnOp("imm", 3), new UnOp("arg", 2))), new BinOp("+", new BinOp("+", new UnOp("imm", 1), new UnOp("imm", 3)), new BinOp("*", new UnOp("imm", 2), new UnOp("imm", 2))));
        Ast p1 = compiler.pass1(prog);
        assertEquals("Pass 1", t1, p1);

        System.out.println(p1);

        // {'op':'/','a':{'op':'-','a':{'op':'+','a':{'op':'*','a':{'op':'imm','n':6},'b':{'op':'arg','n':0}},'b':{'op':'*','a':{'op':'imm','n':5},'b':{'op':'arg','n':1}}},'b':{'op':'*','a':{'op':'imm','n':3},'b':{'op':'arg','n':2}}},'b':{'op':'imm','n':8}}
        Ast t2 = new BinOp("/", new BinOp("-", new BinOp("+", new BinOp("*", new UnOp("imm", 6), new UnOp("arg", 0)), new BinOp("*", new UnOp("imm", 5), new UnOp("arg", 1))), new BinOp("*", new UnOp("imm", 3), new UnOp("arg", 2))), new UnOp("imm", 8));
        Ast p2 = compiler.pass2(p1);
        assertEquals("Pass 2", t2, p2);

        //"[ x y z ] ( 6*x + 5*y - 3*z ) / 8"
        List<String> p3 = compiler.pass3(p2);
        assertEquals("prog(4,0,0) == 3", 3, Simulator.simulate(p3, 4, 0, 0));
        assertEquals("prog(4,8,0) == 8", 8, Simulator.simulate(p3, 4, 8, 0));
        assertEquals("prog(4,8,16) == 2", 2, Simulator.simulate(p3, 4, 8, 16));
    }

    @Test
    public void test1() {
        String p = "[ a b c ] a + b + c";

        Ast actual = compiler.pass1(p);

        Ast expected = new BinOp(
                "+",
                new BinOp(
                        "+",
                        new UnOp("arg", 0),
                        new UnOp("arg", 1)
                ),
                new UnOp("arg", 2)
        );
        assertEquals(expected, actual);
    }

    @Test
    public void test2() {
        String p = "[ a b c ] (a + b) + c";

        Ast actual = compiler.pass1(p);

        Ast expected = new BinOp(
                "+",
                new BinOp(
                        "+",
                        new UnOp("arg", 0),
                        new UnOp("arg", 1)
                ),
                new UnOp("arg", 2)
        );
        assertEquals(expected, actual);
    }

    @Test
    public void test3() {
        String p = "[ a b ] a*a + b*b";

        Ast actual = compiler.pass1(p);

        Ast expected = new BinOp(
                "+",
                new BinOp(
                        "*",
                        new UnOp("arg", 0),
                        new UnOp("arg", 0)
                ),
                new BinOp(
                        "*",
                        new UnOp("arg", 1),
                        new UnOp("arg", 1)
                )
        );
        assertEquals(expected, actual);
    }

    @Test
    public void test4() {
        String prog = "[ x y z ] ( 2*3*x + 5*y - 3*z ) / (1 + 3 + 2*2)";

        // {'op':'/','a':{'op':'-','a':{'op':'+','a':{'op':'*','a':{'op':'*','a':{'op':'imm','n':2},'b':{'op':'imm','n':3}},'b':{'op':'arg','n':0}},'b':{'op':'*','a':{'op':'imm','n':5},'b':{'op':'arg','n':1}}},'b':{'op':'*','a':{'op':'imm','n':3},'b':{'op':'arg','n':2}}},'b':{'op':'+','a':{'op':'+','a':{'op':'imm','n':1},'b':{'op':'imm','n':3}},'b':{'op':'*','a':{'op':'imm','n':2},'b':{'op':'imm','n':2}}}}
        Ast expected = new BinOp("/", new BinOp("-", new BinOp("+", new BinOp("*", new BinOp("*", new UnOp("imm", 2), new UnOp("imm", 3)), new UnOp("arg", 0)), new BinOp("*", new UnOp("imm", 5), new UnOp("arg", 1))), new BinOp("*", new UnOp("imm", 3), new UnOp("arg", 2))), new BinOp("+", new BinOp("+", new UnOp("imm", 1), new UnOp("imm", 3)), new BinOp("*", new UnOp("imm", 2), new UnOp("imm", 2))));
        Ast actual = compiler.pass1(prog);
        assertEquals("Pass 1", expected, actual);
    }

    @Test
    public void test5() {
        String prog = "[ x ] x + 2*5";

        List<String> actual = compiler.compile(prog);

        assertEquals(Arrays.asList("IM 10", "SW", "AR 0", "AD"), actual);
    }

    @Test
    public void test6() {
        String prog = "[ x y ] x*10 + y*2";

        List<String> actual = compiler.compile(prog);

        List<String> expected = Arrays.asList(
                "IM 10",        //un-op
                "SW",           //leaf bin-op [in-between]
                "AR 0",         //un-op
                "MU",           //leaf bin-op [the-end]
                "PU",           //root bin-op [in-between]
                "IM 2",         //un-op
                "SW",           //leaf bin-op [in-between]
                "AR 1",         //un-op
                "MU",           //leaf bin-op [the-end]
                "SW", "PO", "AD"//root bin-op [the-end]
        );

        List<String> expected2 = Arrays.asList(
                "IM 10", //un-op
                "PU",    //leaf bin-op
                "AR 0",  //un-op
                "SW", "PO", "MU", //leaf bin-op
                "PU", //root bin-op
                "IM 2", //un-op
                "PU", //leaf bin-op
                "AR 1", //un-op
                "SW", "PO", "MU", //leaf bin-op
                "SW", "PO", "AD" //root bin-op
        );

        assertEquals(22, Simulator.simulate(actual, 2, 1));
    }
}
