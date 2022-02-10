package codewars;

import java.util.Objects;

public final class BinOp implements Ast {
    private final String op;
    private final Ast a;
    private final Ast b;

    public BinOp(String op, Ast a, Ast b) {
        this.op = op;
        this.a = a;
        this.b = b;
    }

    @Override
    public String op() {
        return op;
    }

    public Ast a() {
        return a;
    }

    public Ast b() {
        return b;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        BinOp binOp = (BinOp) o;
        return Objects.equals(op, binOp.op) &&
                Objects.equals(a, binOp.a) &&
                Objects.equals(b, binOp.b);
    }

    @Override
    public int hashCode() {
        return Objects.hash(op, a, b);
    }

    @Override
    public String toString() {
        return String.format(
                "{'op':'%s','a':%s,'b':%s}",
                op, a, b
        );
    }
}