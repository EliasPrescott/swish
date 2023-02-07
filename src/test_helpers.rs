use crate::{
    ast::{AstFunction, AstFunctionBody, AstNode},
    ast_type::{AstType, MonoType, PolyType},
    std_functions,
};

pub fn float(float: f64) -> AstNode {
    AstNode::Float(float)
}

pub fn int(int: i64) -> AstNode {
    AstNode::Integer(int)
}

pub const unit: AstNode = AstNode::Unit;

pub fn ident(text: &str, expected_type: AstType) -> AstNode {
    AstNode::Identifier(text.to_owned(), expected_type)
}

pub fn func(param: &str, expected_param_type: AstType, body: AstNode) -> AstNode {
    AstNode::Function(AstFunction {
        parameter: param.to_owned(),
        parameter_type: expected_param_type,
        body: AstFunctionBody::Ast(Box::new(body)),
    })
}

pub fn func_call(items: Vec<AstNode>) -> AstNode {
    AstNode::FunctionCall(items)
}

pub fn assert_parse(input: &str, comp: AstNode) {
    let (_, result) = AstNode::parse(input).unwrap();
    assert_eq!(result, comp);
}

pub fn assert_type(input: &str, t: AstType) {
    let (_, result) = AstNode::parse(input).unwrap();
    assert_eq!(result.get_type(&std_functions::std_functions()), t);
}

pub fn parse_type(input: &str, t: AstType) {
    let (_, result) = AstType::parse(input).unwrap();
    assert_eq!(result, t);
}

pub fn poly_type(name: &str) -> AstType {
    AstType::Poly(PolyType {
        name: name.to_owned(),
        bounds: vec![],
    })
}

pub fn bounded_poly_type(name: &str, bounds: Vec<AstType>) -> AstType {
    AstType::Poly(PolyType {
        name: name.to_owned(),
        bounds: bounds,
    })
}

pub fn func_type(parameter_type: AstType, body_type: AstType) -> AstType {
    AstType::Mono(MonoType::Function(
        Box::new(parameter_type),
        Box::new(body_type),
    ))
}

pub const int_type: AstType = AstType::Mono(MonoType::Integer);
pub const float_type: AstType = AstType::Mono(MonoType::Float);
pub const string_type: AstType = AstType::Mono(MonoType::String);
pub const unit_type: AstType = AstType::Mono(MonoType::Unit);
