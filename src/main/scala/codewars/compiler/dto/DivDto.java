package codewars.compiler.dto;

public class DivDto extends BranchAD {
    public DivDto(AstDto a, AstDto b) {
        super("/", a, b);
    }
}
