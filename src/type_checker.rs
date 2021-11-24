use std::collections::HashMap;
use std::string::ToString;

use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum MirandaType {
    Bool,
    Int,
    Float,
    List(Box<MirandaType>),
    Char,
    String,
}

impl ToString for MirandaType {
    fn to_string(&self) -> String {
        match self {
            MirandaType::Bool => "bool".to_string(),
            MirandaType::Int => "int".to_string(),
            MirandaType::Float => "float".to_string(),
            MirandaType::Char => "char".to_string(),
            MirandaType::String => "string".to_string(),
            MirandaType::List(x) => "list ".to_string() + &(*x).to_string(),
        }
    }
}

pub type Ident = String;

// x :: char

#[derive(Clone)]
pub struct VarType(pub Ident, pub MirandaType);

impl VarType {
    pub fn new(id: Ident, t: MirandaType) -> Self {
        Self(id, t)
    }
}

// add :: int -> int -> int
#[derive(Clone)]
pub struct FunType(pub Ident, pub Vec<MirandaType>);

impl FunType {
    pub fn new(id: Ident, t: Vec<MirandaType>) -> Self {
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

pub type FunTable = HashMap<Ident, FunType>;

pub type VarTable = HashMap<Ident, VarType>;

pub fn function_lookup(funcs: &FunTable, identifier: Ident) -> Option<FunType> {
    if let Some(x) = funcs.get(&identifier) {
        return Some((*x).clone());
    }
    return None;
}

pub fn variable_lookup(vars: &VarTable, identifier: Ident) -> Option<VarType> {
    if let Some(x) = vars.get(&identifier) {
        return Some((*x).clone());
    }
    return None;
}

mod tests{
    use super::*;

    #[test]
    fn type_string() {
        let value: MirandaType = MirandaType::List(Box::new(MirandaType::Int));
        assert_eq!(value.to_string(), "list int")
    }
}
