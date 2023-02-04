use core::panic;
use std::{collections::HashMap, fmt::Debug};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0, multispace1, newline},
    combinator::{map, opt, recognize},
    multi::{many0, many1},
    sequence::{delimited, tuple},
    IResult,
};

use crate::{
    ast_type::{AstType, MonoType, PolyType},
    identifier_parser::identifier,
    numeric_parsers::{decimal, float},
    string_parser::parse_string_raw,
};

#[derive(Debug, PartialEq)]
pub enum AstNode {
    Integer(i64),
    Float(f64),
    String(String),
    Function(AstFunction),
    FunctionCall(Vec<AstNode>),
    Identifier(String, AstType),
    Assignment(String, Box<AstNode>),
    Unit,
}

#[derive(Debug, PartialEq)]
pub struct AstFunction {
    pub parameter: String,
    pub parameter_type: AstType,
    pub body: AstFunctionBody,
}

impl AstFunction {
    pub fn get_body_type(&self, known_functions: &HashMap<String, AstFunction>, params: &[AstNode]) -> AstType {
        let base_type = match &self.body {
            AstFunctionBody::Ast(ast) => AstType::Mono(MonoType::Function(
                Box::new(self.parameter_type.clone()),
                Box::new(ast.get_type(known_functions)),
            )),
            AstFunctionBody::Builtin(_, return_type) => AstType::Mono(MonoType::Function(
                Box::new(self.parameter_type.clone()),
                Box::new(return_type.clone()),
            )),
        };

        println!("Params!: {:#?}", params);

        let new_func_type = params
            .iter()
            .fold(base_type, |folding_type, next_param| {
                match folding_type {
                    AstType::Mono(MonoType::Function(param, body)) => {
                        let param_type = next_param.get_type(known_functions);
                        if param.intersects(&param_type) {
                            match param_type {
                                AstType::Poly(p) => {
                                    body.with_poly_as(p.name, *param)
                                },
                                _ => *body
                            }
                        } else {
                            panic!("function was expecting a {}, but got a {}", param, param_type);
                        }
                    },
                    // I'll need to handle this, later :)
                    AstType::Poly(_) => panic!("You got your poly in my function call!"),
                    literal_mono => panic!("Cannot apply {} to {}", next_param.get_type(known_functions), literal_mono),
                }
            });

        new_func_type
    }
}

pub enum AstFunctionBody {
    Ast(Box<AstNode>),
    Builtin(Box<dyn Fn(Vec<AstNode>) -> AstNode>, AstType),
}

impl Debug for AstFunctionBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ast(arg0) => f.debug_tuple("Ast").field(arg0).finish(),
            Self::Builtin(arg0, arg1) => f.debug_tuple("Builtin").finish(),
        }
    }
}

impl PartialEq for AstFunctionBody {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Ast(l0), Self::Ast(r0)) => l0 == r0,
            (Self::Builtin(l0, l1), Self::Builtin(r0, r1)) => false,
            _ => false,
        }
    }
}

type ParseResult<'a> = IResult<&'a str, AstNode>;

impl AstNode {
    pub fn parse(input: &str) -> ParseResult {
        alt((
            parse_function,
            parse_function_call,
            parse_assignment,
            parse_unit,
            parse_float,
            parse_int,
            parse_string,
            parse_identifier,
        ))(input)
    }

    pub fn get_type(&self, known_functions: &HashMap<String, AstFunction>) -> AstType {
        match self {
            AstNode::Float(_) => AstType::Mono(MonoType::Float),
            AstNode::Integer(_) => AstType::Mono(MonoType::Integer),
            AstNode::String(_) => AstType::Mono(MonoType::String),
            AstNode::Unit => AstType::Mono(MonoType::Unit),
            AstNode::Assignment(_, _) => AstType::Mono(MonoType::Unit),
            AstNode::FunctionCall(items) => match &items[..] {
                [] => panic!("How did a unit get here? :("),
                [_] => panic!("Not allowing no parameter function calls for now"),
                [AstNode::Function(func), rest @ ..] => func.get_body_type(known_functions, rest),
                [AstNode::Identifier(ident, _), rest @ ..] => {
                    let func = known_functions
                        .get(ident)
                        .expect(&format!("Could not find function '{}' for call", ident));
                    func.get_body_type(known_functions, &rest)
                }
                [head, ..] => panic!("Bad function call starting with {:?}", head),
            },
            AstNode::Identifier(ident, t) => {
                if let Some(func) = known_functions.get(ident) {
                    func.get_body_type(known_functions, &vec![])
                } else {
                    t.clone()
                }
            },
            AstNode::Function(func) => func.get_body_type(known_functions, &vec![]),
        }
    }
}

fn parse_function(input: &str) -> ParseResult {
    let (rem, _) = char('(')(input)?;
    let (rem, _) = char('\\')(rem)?;
    let (rem, params) = many1(delimited(multispace0, identifier, multispace0))(rem)?;
    let (rem, _) = multispace0(rem)?;
    let (rem, _) = tag("|>")(rem)?;
    let (rem, _) = multispace0(rem)?;
    let (rem, body) = AstNode::parse(rem)?;
    let (rem, _) = char(')')(rem)?;

    Ok((
        rem,
        params
            .into_iter()
            .rev()
            .fold(body, |prev_body, next_param| {
                println!("{}", next_param);
                AstNode::Function(AstFunction {
                    parameter: next_param.to_owned(),
                    parameter_type: AstType::Poly(PolyType {
                        name: "a".to_owned(),
                        bounds: vec![],
                    }),
                    body: AstFunctionBody::Ast(Box::new(prev_body)),
                })
            }),
    ))
}

fn parse_function_call(input: &str) -> ParseResult {
    let (rem, _) = char('(')(input)?;
    let (rem, out) = map(
        many1(delimited(opt(char(' ')), AstNode::parse, opt(char(' ')))),
        |items| AstNode::FunctionCall(items),
    )(rem)?;
    let (rem, _) = char(')')(rem)?;
    Ok((rem, out))
}

fn parse_int(input: &str) -> ParseResult {
    map(recognize(tuple((opt(char('-')), decimal))), |x| {
        AstNode::Integer(str::parse::<i64>(x).unwrap())
    })(input)
}

fn parse_float(input: &str) -> ParseResult {
    map(float, |x| AstNode::Float(str::parse::<f64>(x).unwrap()))(input)
}

fn parse_string(input: &str) -> ParseResult {
    map(parse_string_raw, AstNode::String)(input)
}

fn parse_identifier(input: &str) -> ParseResult {
    map(identifier, |x| {
        AstNode::Identifier(
            x.to_owned(),
            AstType::Poly(PolyType {
                name: "a".to_owned(),
                bounds: vec![],
            }),
        )
    })(input)
}

fn parse_unit(input: &str) -> ParseResult {
    map(tuple((char('('), char(')'))), |_| AstNode::Unit)(input)
}

fn parse_assignment(input: &str) -> ParseResult {
    map(
        tuple((
            recognize(tag("let")),
            multispace1,
            identifier,
            multispace1,
            char('='),
            multispace1,
            AstNode::parse,
        )),
        |(_, _, ident, _, _, _, value)| AstNode::Assignment(ident.to_owned(), Box::new(value)),
    )(input)
}
