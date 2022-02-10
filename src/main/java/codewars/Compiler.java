package codewars;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

public class Compiler {
    public List<String> compile(String prog) {
        return pass3(pass2(pass1(prog)));
    }

    /**
     * Returns an un-optimized AST
     */
    public Ast pass1(String prog) {
        Deque<String> tokens = tokenize(prog);
        while (!tokens.pop().equals("]"));

        //Parse function args
        final Map<String, Integer> args = new HashMap<>();
        String[] parts = prog.split("\\[")[1].split("\\]");
        String[] origArgs = Stream.of(parts[0].split(" ")).filter(x -> !x.isEmpty()).toArray(String[]::new);
        for (int idx = 0; idx < origArgs.length; idx++) {
            args.put(origArgs[idx], idx);
        }

        //Parse the expression tree
        return new Parser(args, tokens)
                .runEval(null, Arrays.asList("$"), true);
    }

    /**
     * Returns an AST with constant expressions reduced
     */
    public Ast pass2(Ast ast) {
        return toAst(fromAst(ast).simplify());
    }

    /**
     * Returns assembly instructions
     */
    public List<String> pass3(Ast ast) {
        return fromAst(ast).compile();
    }

    private static Deque<String> tokenize(String prog) {
        Deque<String> tokens = new LinkedList<>();
        Pattern pattern = Pattern.compile("[-+*/()\\[\\]]|[a-zA-Z]+|\\d+");
        Matcher m = pattern.matcher(prog);
        while (m.find()) {
            tokens.add(m.group());
        }
        tokens.add("$"); // end-of-stream
        return tokens;
    }

    //My helpers:

    private static Ast toAst(MyAst ast) {
        if (ast instanceof MyUnOp) {
            MyUnOp unOp = (MyUnOp) ast;
            return new UnOp(unOp.op, unOp.arg);
        }

        MyBinOp binOp = (MyBinOp) ast;
        return new BinOp(
                binOp.op,
                toAst(binOp.l),
                toAst(binOp.r)
        );
    }

    private static MyAst fromAst(Ast ast) {
        if (ast instanceof UnOp) {
            UnOp unOp = (UnOp) ast;
            return new MyUnOp(unOp.op(), unOp.n());
        }

        BinOp binOp = (BinOp) ast;
        return new MyBinOp(
                binOp.op(),
                fromAst(binOp.a()),
                fromAst(binOp.b())
        );
    }

    //My classes:

    private static class Parser {
        private final Map<String, Integer> args;
        private final Deque<String> tokens;

        private Parser(Map<String, Integer> args, Deque<String> tokens) {
            this.args = args;
            this.tokens = tokens;
        }

        private Ast runEval(Ast left, List<String> terms, boolean doPop) {
            if (terms.contains(tokens.peek())) {
                if (doPop) {
                    tokens.pop();
                }
                return left;
            }

            Ast newLeft = evalExpr(left);
            return runEval(newLeft, terms, doPop);
        }

        private Ast evalExpr(Ast left) {
            String head = tokens.pop();

            switch (head) {
                case "(": return runEval(null, Arrays.asList(")"), true);
                case "/":
                case "*": return new BinOp(head, left, evalExpr(null));
                case "+":
                case "-": return new BinOp(
                        head,
                        left,
                        runEval(null, Arrays.asList("+", "-", ")", "$"), false) //eval term
                );
                default: {
                    if (args.containsKey(head)) {
                        return new UnOp("arg", args.get(head));
                    } else {
                        return new UnOp("imm", Integer.parseInt(head));
                    }
                }
            }
        }
    }

    interface MyAst extends Ast {
        MyAst simplify();
        List<String> compile();
    }

    private static final class MyBinOp implements MyAst {
        private final String op;
        private final MyAst l;
        private final MyAst r;

        public MyBinOp(String op, MyAst l, MyAst r) {
            this.op = op;
            this.l = l;
            this.r = r;
        }

        @Override
        public MyAst simplify() {
            MyAst newL = l.simplify();
            MyAst newR = r.simplify();

            if (!newL.op().equals("imm") || !newR.op().equals("imm")) {
                return new MyBinOp(op, newL, newR);
            }

            MyUnOp newL1 = (MyUnOp) newL;
            MyUnOp newR1 = (MyUnOp) newR;

            switch (op) {
                case "+": return new MyUnOp("imm", newL1.arg + newR1.arg);
                case "-": return new MyUnOp("imm", newL1.arg - newR1.arg);
                case "/": return new MyUnOp("imm", newL1.arg / newR1.arg);
                case "*": return new MyUnOp("imm", newL1.arg * newR1.arg);
                default: throw new IllegalStateException("unknown BinOp's op");
            }
        }

        @Override
        public List<String> compile() {
            List<String> resL = l.compile();
            List<String> resR = r.compile();

            List<String> result = new LinkedList<>(resL);
            result.add("PU");
            result.addAll(resR);
            result.add("SW");
            result.add("PO");

            switch (op) {
                case "+": result.add("AD"); break;
                case "-": result.add("SU"); break;
                case "*": result.add("MU"); break;
                case "/": result.add("DI"); break;
            }

            return result;
        }

        @Override
        public String op() {
            return op;
        }
    }

    private static final class MyUnOp implements MyAst {
        private final String op;
        private final int arg;

        public MyUnOp(String op, int arg) {
            this.op = op;
            this.arg = arg;
        }

        @Override
        public List<String> compile() {
            String cmd;

            switch (op) {
                case "imm": cmd = "IM %d"; break;
                case "arg": cmd = "AR %d"; break;
                default: throw new IllegalStateException("unknown UnOp's op");
            }

            return Collections.singletonList(String.format(cmd, arg));
        }

        @Override
        public MyAst simplify() {
            return this;
        }

        @Override
        public String op() {
            return op;
        }
    }
}