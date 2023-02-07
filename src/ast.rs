use core::panic;
use std::{collections::HashMap, fmt::Debug, rc::Rc};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0, multispace1, newline},
    combinator::{map, opt, recognize},
    multi::{many0, many1, many_till},
    sequence::{delimited, tuple},
    IResult,
};

use crate::{
    ast_type::{AstType, MonoType, PolyType},
    identifier_parser::identifier,
    numeric_parsers::{decimal, float},
    string_parser::parse_string_raw,
};

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub struct AstFunction {
    pub parameter: String,
    pub parameter_type: AstType,
    pub body: AstFunctionBody,
}

impl AstFunction {
    pub fn get_body_type(
        &self,
        known_functions: &HashMap<String, AstFunction>,
        params: &[AstNode],
    ) -> AstType {
        let base_type = match &self.body {
            AstFunctionBody::Ast(ast) => {
                let ast = ast.clone();
                AstType::Mono(MonoType::Function(
                    Box::new(self.parameter_type.clone()),
                    Box::new(ast.replace_identifier_type(&self.parameter, self.parameter_type.clone()).0.get_type(known_functions)),
                ))
            },
            AstFunctionBody::Builtin(_, return_type) => AstType::Mono(MonoType::Function(
                Box::new(self.parameter_type.clone()),
                Box::new(return_type.clone()),
            )),
        };

        let new_func_type = params.iter().fold(base_type, |folding_type, next_param| {
            match folding_type {
                AstType::Mono(MonoType::Function(param, body)) => {
                    let param_type = next_param.get_type(known_functions);
                    match param.intersection(&param_type) {
                        Some(new_intersection) => match (*param, param_type) {
                            (AstType::Poly(a), AstType::Poly(b)) => body
                                .with_poly_as(a.name, new_intersection.clone())
                                .with_poly_as(b.name, new_intersection),
                            (AstType::Poly(a), _) | (_, AstType::Poly(a)) => {
                                body.with_poly_as(a.name, new_intersection)
                            }
                            _ => *body,
                        },
                        None => panic!(
                            "function was expecting a {}, but got a {}",
                            param, param_type
                        ),
                    }
                    // if param.intersects(&param_type) {
                    //     match param_type {
                    //         AstType::Poly(p) => {
                    //             body.with_poly_as(p.name, *param)
                    //         },
                    //         _ => *body
                    //     }
                    // } else {
                    //     panic!("function was expecting a {}, but got a {}", param, param_type);
                    // }
                }
                // I'll need to handle this, later :)
                AstType::Poly(_) => panic!("You got your poly in my function call!"),
                literal_mono => panic!(
                    "Cannot apply {} to {}",
                    next_param.get_type(known_functions),
                    literal_mono
                ),
            }
        });

        new_func_type
    }
}

#[derive(Clone)]
pub enum AstFunctionBody {
    Ast(Box<AstNode>),
    Builtin(Rc<dyn Fn(Vec<AstNode>) -> AstNode>, AstType),
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
            parse_typed_identifier,
        ))(input)
    }

    pub fn get_first_identifier_usage(&self, identifier: &str) -> Option<&Self> {
        match self {
            AstNode::Identifier(ident, _) => {
                if ident == identifier {
                    Some(self)
                } else {
                    None
                }
            },
            AstNode::Function(func) => {
                match &func.body {
                    AstFunctionBody::Ast(body) => body.get_first_identifier_usage(identifier),
                    AstFunctionBody::Builtin(_, _) => None,
                }
            },
            AstNode::FunctionCall(items) => {
                items.iter().find_map(|x| x.get_first_identifier_usage(identifier))
            },
            other => None,
        }
    }

    pub fn replace_identifier_type(self, identifier: &str, new_type: AstType) -> (Self, Option<AstType>) {
        match self {
            AstNode::Identifier(ident, t) => (AstNode::Identifier(
                ident.clone(),
                if identifier == ident { new_type } else { t.clone() },
            ), Some(t)),
            AstNode::Function(func) => {
                let (new_func_body, bubble_type) = match func.body {
                    AstFunctionBody::Ast(b) => {
                        let (bod, bub) = b.replace_identifier_type(identifier, new_type);
                        (AstFunctionBody::Ast(Box::new(
                            bod,
                        )), bub)
                    },
                    AstFunctionBody::Builtin(b, t) => (AstFunctionBody::Builtin(b, t), None),
                };

                (AstNode::Function(AstFunction {
                    parameter: func.parameter,
                    parameter_type: func.parameter_type,
                    body: new_func_body,
                }), bubble_type)
            }
            AstNode::FunctionCall(items) => {
                let mapped_items: Vec<_> = items
                    .into_iter()
                    .map(|x| x.replace_identifier_type(identifier, new_type.clone()))
                    .collect();
                let first_bubble_type = mapped_items
                    .iter()
                    .find(|(_, y)| y.is_some())
                    .and_then(|x| x.1.clone());
                (AstNode::FunctionCall(mapped_items.into_iter().map(|(x, _)| x).collect()), first_bubble_type)
            },
            other => (other, None),
        }
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
            }
            AstNode::Function(func) => func.get_body_type(known_functions, &vec![]),
        }
    }
}

fn parse_function(input: &str) -> ParseResult {
    let (rem, _) = char('(')(input)?;
    let (rem, _) = char('\\')(rem)?;
    let (rem, (params, _)) = many_till(
        delimited(
            multispace0,
            alt((
                map(parse_typed_identifier_raw, |(s, t)| (s, Some(t))),
                map(identifier, |s| (s.to_owned(), None)),
            )),
            multispace0,
        ),
        tag("->"),
    )(rem)?;
    // let (rem, _) = multispace0(rem)?;
    // let (rem, _) = tag("->")(rem)?;
    let (rem, _) = multispace0(rem)?;
    let (rem, body) = AstNode::parse(rem)?;
    let (rem, _) = char(')')(rem)?;

    Ok((
        rem,
        params
            .into_iter()
            .rev()
            .fold(body, |prev_body, (next_ident, next_ident_type)| {
                AstNode::Function(AstFunction {
                    parameter: next_ident.to_owned(),
                    parameter_type: match next_ident_type {
                        Some(t) => t,
                        None => AstType::Poly(PolyType {
                            name: "a".to_owned(),
                            bounds: vec![],
                        }),
                    },
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

fn parse_typed_identifier(input: &str) -> ParseResult {
    map(parse_typed_identifier_raw, |(ident, t)| {
        AstNode::Identifier(ident.to_owned(), t)
    })(input)
}

fn parse_typed_identifier_raw(input: &str) -> IResult<&str, (String, AstType)> {
    map(
        tuple((
            char('('),
            identifier,
            multispace0,
            char(':'),
            multispace0,
            AstType::parse,
            char(')'),
        )),
        |(_, ident, _, _, _, t, _)| (ident.to_owned(), t),
    )(input)
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
