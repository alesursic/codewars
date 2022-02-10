package codewars.compiler.dto;

public class LeafAD extends AstDto {
    private int n;

    public LeafAD(String op, int n) {
        super(op);
        this.n = n;
    }

    public int getN() {
        return n;
    }

    public void setN(int n) {
        this.n = n;
    }
}
