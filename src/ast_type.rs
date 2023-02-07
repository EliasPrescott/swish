use std::{
    collections::HashSet,
    fmt::{Debug, Display},
};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, char, multispace0, multispace1},
    combinator::map,
    sequence::tuple,
    IResult,
};

use crate::identifier_parser::identifier;

#[derive(PartialEq, Clone)]
pub enum AstType {
    Mono(MonoType),
    Poly(PolyType),
}

#[derive(Debug, PartialEq, Clone)]
pub enum MonoType {
    Integer,
    Float,
    String,
    Function(Box<AstType>, Box<AstType>),
    Unit,
}

#[derive(Debug, PartialEq, Clone)]
pub struct PolyType {
    pub name: String,
    pub bounds: Vec<AstType>,
}

impl AstType {
    pub fn parse(input: &str) -> IResult<&str, AstType> {
        alt((
            parse_int_type,
            parse_func_type,
            parse_float_type,
            parse_unit_type,
            parse_string_type,
            parse_unbound_poly_type,
        ))(input)
    }

    pub fn intersects(&self, other: &Self) -> bool {
        match (self, other) {
            (AstType::Mono(a), AstType::Mono(b)) => a.intersects(b),
            (a, AstType::Poly(b)) | (AstType::Poly(b), a) => {
                if b.bounds.is_empty() {
                    true
                } else {
                    b.bounds.contains(a)
                }
            }
        }
    }

    pub fn intersection(&self, other: &Self) -> Option<AstType> {
        match (self, other) {
            (AstType::Mono(a), AstType::Mono(b)) => a.intersection(b).map(AstType::Mono),
            (AstType::Mono(a), AstType::Poly(b)) | (AstType::Poly(b), AstType::Mono(a)) => {
                if b.bounds.is_empty() {
                    Some(AstType::Mono(a.clone()))
                } else {
                    if b.bounds.contains(&AstType::Mono(a.clone())) {
                        Some(AstType::Mono(a.clone()))
                    } else {
                        None
                    }
                }
            }
            (AstType::Poly(a), AstType::Poly(b)) => {
                if a.bounds.is_empty() {
                    Some(AstType::Poly(b.clone()))
                } else if b.bounds.is_empty() {
                    Some(AstType::Poly(a.clone()))
                }
                else {
                    let mut intersections = vec![];
                    for bound in &a.bounds[..] {
                        if b.bounds.contains(&bound) {
                            intersections.push(bound.clone());
                        }
                    }
                    if intersections.is_empty() {
                        None
                    } else {
                        Some(AstType::Poly(PolyType {
                            name: a.name.clone(),
                            bounds: intersections,
                        }))
                    }
                }
            }
        }
    }

    pub fn with_poly_as(self, poly_name: String, as_type: AstType) -> Self {
        match self {
            AstType::Mono(MonoType::Function(p, body)) => {
                // match *p {
                //     AstType::Poly(poly_param) => {
                //         if poly_param.name == poly_name {
                //             AstType::Mono(MonoType::Function(Box::new(as_type), body))
                //         } else {
                //             AstType::Mono(MonoType::Function(Box::new(AstType::Poly(poly_param)), body))
                //         }
                //     },
                //     _ => AstType::Mono(MonoType::Function(p, Box::new(body.with_poly_as(poly_name, as_type))))
                // }
                AstType::Mono(MonoType::Function(
                    Box::new(p.with_poly_as(poly_name.clone(), as_type.clone())),
                    Box::new(body.with_poly_as(poly_name, as_type)),
                ))
            }
            AstType::Poly(p) => {
                if p.name == poly_name {
                    as_type
                } else {
                    AstType::Poly(PolyType {
                        name: p.name,
                        bounds: p
                            .bounds
                            .into_iter()
                            .map(|x| x.with_poly_as(poly_name.clone(), as_type.clone()))
                            .collect(),
                    })
                }
            }
            other => other,
        }
    }
}

impl MonoType {
    fn intersects(&self, other: &Self) -> bool {
        match (self, other) {
            (MonoType::Function(param1, body1), MonoType::Function(param2, body2)) => {
                param1.intersects(param2) && body1.intersects(body2)
            }
            (a, b) => a == b,
        }
    }

    fn intersection(&self, other: &Self) -> Option<MonoType> {
        match (self, other) {
            (MonoType::Function(param1, body1), MonoType::Function(param2, body2)) => {
                if param1.intersects(param2) && body1.intersects(body2) {
                    Some(MonoType::Function(param1.clone(), body1.clone()))
                } else {
                    None
                }
            }
            (a, b) => {
                if a == b {
                    Some(a.clone())
                } else {
                    None
                }
            }
        }
    }
}

impl Debug for AstType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self, f)
    }
}

impl Display for AstType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstType::Mono(m) => match m {
                MonoType::Float => f.write_str("float"),
                MonoType::Integer => f.write_str("int"),
                MonoType::String => f.write_str("string"),
                MonoType::Unit => f.write_str("()"),
                MonoType::Function(a, b) => f.write_fmt(format_args!("{} -> {}", a, b)),
            },
            AstType::Poly(p) => {
                if p.bounds.is_empty() {
                    f.write_fmt(format_args!("'{}", p.name))
                } else {
                    f.write_fmt(format_args!(
                        "('{}: {})",
                        p.name,
                        p.bounds
                            .iter()
                            .map(|x| x.to_string())
                            .collect::<Vec<String>>()
                            .join(", ")
                    ))
                }
            }
        }
    }
}

pub fn parse_int_type(input: &str) -> IResult<&str, AstType> {
    map(tag("int"), |_| AstType::Mono(MonoType::Integer))(input)
}

pub fn parse_float_type(input: &str) -> IResult<&str, AstType> {
    map(tag("float"), |_| AstType::Mono(MonoType::Float))(input)
}

pub fn parse_unit_type(input: &str) -> IResult<&str, AstType> {
    map(tag("()"), |_| AstType::Mono(MonoType::Unit))(input)
}

pub fn parse_string_type(input: &str) -> IResult<&str, AstType> {
    map(tag("string"), |_| AstType::Mono(MonoType::String))(input)
}

pub fn parse_unbound_poly_type(input: &str) -> IResult<&str, AstType> {
    let (rem, _) = char('\'')(input)?;
    let (rem, name) = alpha1(rem)?;
    Ok((
        rem,
        AstType::Poly(PolyType {
            name: name.to_owned(),
            bounds: vec![],
        }),
    ))
}

pub fn parse_func_type(input: &str) -> IResult<&str, AstType> {
    map(
        tuple((
            char('('),
            AstType::parse,
            multispace0,
            tag("->"),
            multispace0,
            AstType::parse,
            char(')'),
        )),
        |(_, param, _, _, _, out, _)| {
            AstType::Mono(MonoType::Function(Box::new(param), Box::new(out)))
        },
    )(input)
}
