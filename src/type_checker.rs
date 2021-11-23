use std::collections::HashMap;
use std::string::ToString;

use std::fmt;

#[derive(Clone)]
pub enum Type {
    Int,
    Bool,
    Float,
    Char,
    String,
    List(Box<Type>),
}

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::Bool => "bool".to_string(),
            Type::Int => "int".to_string(),
            Type::Float => "float".to_string(),
            Type::Char => "char".to_string(),
            Type::String => "string".to_string(),
            Type::List(x) => "list ".to_string() + &(*x).to_string(),
        }
    }
}

pub type Ident = String;

#[derive(Clone)]
pub struct VarType(pub Ident, pub Type);

impl VarType {
    pub fn new(id: Ident, t: Type) -> Self {
        Self(id, t)
    }
}

#[derive(Clone)]
pub struct FunType(pub Ident, pub Vec<Type>);

impl FunType {
    pub fn new(id: Ident, t: Vec<Type>) -> Self {
        Self(id, t)
    }
}

impl fmt::Display for FunType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Function ")?;
        for v in &self.1 {
            write!(f, "{} ", v.to_string())?;
        }
        write!(f, "already defined")
    }
}

pub type FunTable = HashMap<String, FunType>;

pub type VarTable = HashMap<String, VarType>;

pub fn look_up_function(funcs: FunTable, identifier: Ident) -> Option<FunType> {
    if let Some(x) = funcs.get(&identifier) {
        return Some((*x).clone());
    }
    return None;
}

pub fn look_up_variable(vars: VarTable, identifier: Ident) -> Option<VarType> {
    if let Some(x) = vars.get(&identifier) {
        return Some((*x).clone());
    }
    return None;
}

mod tests{
    use super::*;

    #[test]
    fn type_string() {
        let value: Type = Type::List(Box::new(Type::Int));
        assert_eq!(value.to_string(), "list int")
    }
}
