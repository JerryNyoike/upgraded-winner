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

#[derive(Clone, PartialEq, Debug)]
pub struct VarType(pub Ident, pub MirandaType);

impl VarType {
    pub fn new(id: Ident, t: MirandaType) -> Self {
        Self(id, t)
    }
}

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

pub type FunTable = HashMap<Ident, FunType>;
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

pub struct Env {
    funs_table: FunTable,
    vars_table: VarTable,
}

impl Env {
    pub fn function_lookup(&self, identifier: &Ident) -> Option<FunType> {
        if let Some(x) = self.funs_table.get(identifier) {
            return Some((*x).clone());
        }
        return None;
    }

    pub fn variable_lookup(&self, identifier: &Ident) -> Option<VarType> {
        if let Some(x) = self.vars_table.get(identifier) {
            return Some((*x).clone());
        }
        return None;
    }

    // insert a variable to table
    fn extend_var(&mut self, id: Ident, t: MirandaType) {
        if let Some(v) = self.variable_lookup(&id) {
            println!("Variable {} {} already defined", v.0, v.1.to_string());
            return;
        }
        self.vars_table.insert(id.clone(), VarType::new(id, t));
    }

    // insert a function to the table
    fn extend_fn(&mut self, id: Ident, t: Vec<MirandaType>) {
        if let Some(f) = self.function_lookup(&id) {
            println!("Function {} is already defined", f);
            return;
        }
        self.funs_table.insert(id.clone(), FunType::new(id, t));
    }

    // returns false if the name is not found in the symbol table
    // true otherwise
    fn name_lookup(&self, id: Ident) -> bool {
        if let None = self.variable_lookup(&id) {
            if let None = self.function_lookup(&id) {
                return false;
            }
        }
        true
    }

    // checks if variable identifier has specified type
    fn check_var(&self, id: Ident, t: MirandaType) -> bool {
        // if let Some(v) = variable_lookup()
        false
    }
}

mod tests {
    use super::*;

    #[test]
    fn type_string() {
        let value: MirandaType = MirandaType::List(Box::new(MirandaType::Int));
        assert_eq!(value.to_string(), "list int")
    }
}
