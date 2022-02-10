package codewars;

import java.util.Objects;

public final class UnOp implements Ast {
    private final String op;
    private final int n;

    public UnOp(String op, int n) {
        this.op = op;
        this.n = n;
    }

    @Override
    public String op() {
        return op;
    }

    public int n() {
        return n;
    }

    @Override
    public String toString() {
        return String.format(
                "{'op':'%s','n':%d}",
                op, n
        );
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        UnOp unOp = (UnOp) o;
        return n == unOp.n &&
                Objects.equals(op, unOp.op);
    }

    @Override
    public int hashCode() {
        return Objects.hash(op, n);
    }
}