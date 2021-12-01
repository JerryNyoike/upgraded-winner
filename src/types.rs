use std::collections::HashMap;
use std::fmt;
use std::string::ToString;

pub type Ident = String;

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

pub trait Bindings {}

#[derive(Clone, PartialEq, Debug)]
pub struct VarType(pub Ident, pub MirandaType);

impl VarType {
    pub fn new(id: Ident, t: MirandaType) -> Self {
        Self(id, t)
    }
}

impl Bindings for VarType{}

#[derive(Clone, PartialEq, Debug)]
pub struct FunType(pub Ident, pub Vec<MirandaType>);

impl FunType {
    pub fn new(id: Ident, t: Vec<MirandaType>) -> Self {
        Self(id, t)
    }
}

impl fmt::Display for FunType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for v in &self.1 {
            write!(f, "{} ", v.to_string())?;
        }
        Ok(())
    }
}

impl Bindings for FunType{}

pub type VarTable = HashMap<Ident, VarType>;


// Supported types for this Miranda
#[derive(Debug, PartialEq, Clone)]
pub enum MirandaExpr {
    MirandaBoolean(bool),
    MirandaInt(i32),
    MirandaFloat(f32),
    MirandaChar(char),
    MirandaString(String),
    MirandaKeyword(Keyword),
    MirandaList(Vec<MirandaExpr>),
    MirandaIdentifier(String),
    MirandaIf(String),
    MirandaBinding(VarType, Box<MirandaExpr>),
    MirandaFunction(FunType, Vec<VarType>, Vec<Vec<MirandaExpr>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum MirandaFunc {
    UserDefined(UserFunc),
    CoreFunc,
}

#[derive(Debug, Clone)]
pub enum BuiltIn {
    Plus,
    Minus,
    Times,
    Divide,
    Equal,
    Mod,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Where,
    If,
    Otherwise,
    Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UserFunc {
    frame_id: u32,
}

mod tests {
    use super::*;

    #[test]
    fn test_type_string() {
        let value: MirandaType = MirandaType::List(Box::new(MirandaType::Int));
        assert_eq!(value.to_string(), "list int")
    }
}
