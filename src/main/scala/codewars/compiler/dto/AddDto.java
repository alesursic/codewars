package codewars.compiler.dto;

public class AddDto extends BranchAD {
    public AddDto(AstDto a, AstDto b) {
        super("+", a, b);
    }
}
