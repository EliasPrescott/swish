use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{AstFunction, AstFunctionBody, AstNode},
    ast_type::{AstType, MonoType, PolyType},
};

pub fn std_functions() -> HashMap<String, AstFunction> {
    [(
        "+".to_owned(),
        AstFunction {
            parameter: "x".to_owned(),
            parameter_type: AstType::Poly(PolyType {
                name: "a".to_owned(),
                bounds: vec![
                    AstType::Mono(MonoType::Integer),
                    AstType::Mono(MonoType::Float),
                    AstType::Mono(MonoType::String),
                ],
            }),
            body: AstFunctionBody::Ast(Box::new(AstNode::Function(AstFunction {
                parameter: "y".to_owned(),
                parameter_type: AstType::Poly(PolyType {
                    name: "a".to_owned(),
                    bounds: vec![
                        AstType::Mono(MonoType::Integer),
                        AstType::Mono(MonoType::Float),
                        AstType::Mono(MonoType::String),
                    ],
                }),
                body: AstFunctionBody::Builtin(
                    Rc::new(|params| match &params[..] {
                        [AstNode::Integer(x), AstNode::Integer(y)] => AstNode::Integer(x + y),
                        [AstNode::Float(x), AstNode::Float(y)] => AstNode::Float(x + y),
                        [AstNode::String(x), AstNode::String(y)] => {
                            AstNode::String(x.to_owned() + y)
                        }
                        _ => panic!("Bad call to builtin func: +"),
                    }),
                    AstType::Poly(PolyType {
                        name: "a".to_owned(),
                        bounds: vec![
                            AstType::Mono(MonoType::Integer),
                            AstType::Mono(MonoType::Float),
                            AstType::Mono(MonoType::String),
                        ],
                    }),
                ),
            }))),
        },
    )]
    .into()
}
