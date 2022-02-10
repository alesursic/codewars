package codewars.compiler.dto;

public class AstDto {
    private String op;

    public AstDto(String op) {
        this.op = op;
    }

    public String getOp() {
        return op;
    }

    public void setOp(String op) {
        this.op = op;
    }
}
