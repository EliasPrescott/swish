use ast::AstNode;

use crate::std_functions::std_functions;

mod ast;
mod ast_type;
mod identifier_parser;
mod numeric_parsers;
mod std_functions;
mod string_parser;
mod test_helpers;

fn main() {
    let (remaining, result) = AstNode::parse(r#"(+ x y)"#).unwrap();
    println!("Remaining input = '{}'", remaining);
    println!("Result = {:#?}", result);
    println!(
        "Result Type = {:#?}",
        result.get_type(&std_functions::std_functions())
    );
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::*;

    #[test]
    fn parse_float() {
        assert_parse("0.25", float(0.25));
        assert_parse("-99.995", float(-99.995));
        assert_parse("25e25", float(25e25));
    }

    #[test]
    fn parse_integer() {
        assert_parse("25", int(25));
        assert_parse("-995", int(-995));
        assert_parse("0", int(0));
    }

    #[test]
    fn parse_unit() {
        assert_parse("()", unit);
    }

    #[test]
    fn inference_tests() {
        assert_type("(\\x -> x)", func_type(poly_type("a"), poly_type("a")));

        assert_type("(\\x -> 25)", func_type(poly_type("a"), int_type));

        assert_type(
            "(\\x y -> ())",
            func_type(poly_type("a"), func_type(poly_type("a"), unit_type)),
        );

        let addable = bounded_poly_type("a", vec![int_type, float_type, string_type]);

        assert_type(
            "+",
            func_type(addable.clone(), func_type(addable.clone(), addable.clone())),
        );

        assert_type("(+ 5)", func_type(int_type, int_type));

        assert_type("(\\(x: int) -> x)", func_type(int_type, int_type));

        assert_type("((\\x -> x) 5)", int_type);

        assert_type("(\\z -> (+ 1 z))", func_type(int_type, int_type));

        assert_type("(x: int)", int_type);

        assert_type("(+ \"prefix-\")", func_type(string_type, string_type));
    }

    #[test]
    fn parse_types() {
        parse_type("int", int_type);
        parse_type("string", string_type);
        parse_type("float", float_type);
        parse_type(
            "(int -> (int -> int))",
            func_type(int_type, func_type(int_type, int_type)),
        );
    }
}
