package codewars.compiler.dto;

public class BranchAD extends AstDto {
    private AstDto a;
    private AstDto b;

    public BranchAD(String op, AstDto a, AstDto b) {
        super(op);
        this.a = a;
        this.b = b;
    }

    public AstDto getA() {
        return a;
    }

    public void setA(AstDto a) {
        this.a = a;
    }

    public AstDto getB() {
        return b;
    }

    public void setB(AstDto b) {
        this.b = b;
    }
}
