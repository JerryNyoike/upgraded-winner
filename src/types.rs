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
    Fun(Vec<MirandaType>),
    Nil,
}

#[derive(PartialEq, Debug)]
pub enum TypeError {
    Mismatch,
    NotAFunction,
    NotInScope,
}

pub fn check(expr: &MirandaExpr, env: &Env) -> Result<MirandaType, TypeError> {
    match expr {
        MirandaExpr::MirandaInt(_) => Ok(MirandaType::Int),
        MirandaExpr::MirandaBoolean(_) => Ok(MirandaType::Bool),
        MirandaExpr::MirandaList(elems) => {
            let elem_type = check(&elems[0].clone(), env);
            match elem_type {
                Ok(el_typ) => Ok(MirandaType::List(Box::new(el_typ))),
                Err(typ_err) => Err(typ_err),
            }
        }
        MirandaExpr::MirandaChar(_) => Ok(MirandaType::Char),
        MirandaExpr::MirandaString(_) => Ok(MirandaType::String),
        MirandaExpr::MirandaBuiltInExpr(b_expr) => {
            let t1 = check(&b_expr[0].clone(), env).unwrap();
            let t2 = check(&b_expr[2].clone(), env).unwrap();
            match b_expr[1] {
                MirandaExpr::MirandaBuiltIn(BuiltIn::GreaterThan) => Ok(MirandaType::Bool),
                MirandaExpr::MirandaBuiltIn(BuiltIn::LessThan) => Ok(MirandaType::Bool),
                MirandaExpr::MirandaBuiltIn(BuiltIn::Equal) => Ok(MirandaType::Bool),
                _ => {
                    if t1 == t2 {
                        return Ok(t2);
                    }
                    return Err(TypeError::Mismatch);
                }
            }
        }
        MirandaExpr::MirandaIdentifier(ident) => {
            // check if the identifier is bound to something in the environment or function frames
            match env.variable_lookup(ident) {
                Ok(vartype) => Ok(vartype.1),
                Err(_) => Err(TypeError::NotInScope),
            }
        }
        _ => Ok(MirandaType::Nil),
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeError::Mismatch => write!(f, "Type mismatch"),
            TypeError::NotAFunction => write!(f, "Not a function"),
            TypeError::NotInScope => write!(f, "Not in scope"),
        }
    }
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
            MirandaType::Nil => "()".to_string(),
            MirandaType::Fun(typ) => {
                let mut typ_str_arr: Vec<String> = vec![];
                for (n, t) in typ.clone().iter().enumerate() {
                    typ_str_arr.push(t.to_string());
                }
                typ_str_arr.join(" -> ")
            }
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
    MirandaBuiltIn(BuiltIn),
    MirandaKeyword(Keyword),
    MirandaList(Vec<MirandaExpr>),
    MirandaIdentifier(String),
    MirandaIf(Box<MirandaExpr>),
    MirandaBindingDeclaration(VarType),
    MirandaBindingDefinition(Ident, Box<MirandaExpr>),
    MirandaFunctionDeclaration(FunType),
    MirandaFunctionDefinition(Ident, Vec<Ident>, Vec<Vec<MirandaExpr>>),
    MirandaBuiltInExpr(Vec<MirandaExpr>),
}

impl fmt::Display for MirandaExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MirandaExpr::MirandaBoolean(bl) => write!(f, "{}", bl),
            MirandaExpr::MirandaInt(num) => write!(f, "{}", num),
            MirandaExpr::MirandaFloat(fl) => write!(f, "{}", fl),
            MirandaExpr::MirandaChar(ch) => write!(f, "'{}'", ch),
            MirandaExpr::MirandaString(string) => write!(f, "\"{}\"", string),
            MirandaExpr::MirandaList(ls) => {
                let mut ls_as_str = vec![];
                for l in ls {
                    ls_as_str.push(l.to_string());
                }
                write!(f, "{:#?}", ls_as_str)
            }
            _ => todo!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum MirandaFunc {
    UserDefined(UserFunc),
    CoreFunc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltIn {
    Plus,
    Minus,
    Times,
    Divide,
    Equal,
    Mod,
    GreaterThan,
    LessThan,
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
    // frame_id: u32,
    params: Vec<Ident>,
    body: Vec<Vec<MirandaExpr>>,
}

impl UserFunc {
    fn new() -> Self {
        Self {
            params: vec![],
            body: vec![],
        }
    }

    pub fn set_params(&mut self, params: Vec<Ident>) {
        self.params = params
    }

    pub fn set_body(&mut self, body: Vec<Vec<MirandaExpr>>) {
        self.body = body.clone()
    }

    fn clear_params(&mut self) {
        self.params = Vec::new()
    }
}

pub struct Env {
    funs_table: FunTable,
    vars_table: VarTable,
    var_values: HashMap<Ident, Vec<MirandaExpr>>,
    fun_values: HashMap<Ident, UserFunc>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            funs_table: HashMap::new(),
            vars_table: HashMap::new(),
            var_values: HashMap::new(),
            fun_values: HashMap::new(),
        }
    }

    pub fn function_lookup(&self, identifier: &Ident) -> Result<FunType, TypeError> {
        if let Some(x) = self.funs_table.get(identifier) {
            return Ok((*x).clone());
        }
        Err(TypeError::NotInScope)
    }

    pub fn variable_lookup(&self, identifier: &Ident) -> Result<VarType, TypeError> {
        if let Some(x) = self.vars_table.get(identifier) {
            return Ok((*x).clone());
        }
        Err(TypeError::NotInScope)
    }

    pub fn function_body(&self, identifier: &Ident) -> Option<Vec<MirandaExpr>> {
        if let Some(x) = self.funs_table.get(identifier) {
            match x {
                FunType(id, _) => {
                    // get the function body from the environment
                    if let Some(fun_body) = self.fun_values.get(id) {
                        Some(fun_body)
                    } else {
                        println!("Function declared but not defined");
                        None
                    };
                }
            }
        }
        println!("Function {} not defined", identifier);
        None
    }

    pub fn binding_value(&self, identifier: &Ident) -> Option<MirandaExpr> {
        if let Some(x) = self.vars_table.get(identifier) {
            match x {
                VarType(id, _) => {
                    // get the function body from the environment
                    if let Some(var_value) = self.var_values.get(id) {
                        Some(var_value)
                    } else {
                        println!("Variable {} declared but not defined", identifier);
                        None
                    };
                }
            }
        }
        println!("Variable {} not defined", identifier);
        None
    }

    // insert a variable to table
    pub fn extend_var(&mut self, id: Ident, t: MirandaType) {
        match self.variable_lookup(&id) {
            Ok(v) => {
                println!("Variable {} {} already defined", v.0, v.1.to_string());
                return;
            }
            Err(_) => {
                self.vars_table.insert(id.clone(), VarType::new(id, t));
            }
        }
    }

    // set the value of the variable
    pub fn set_var_value(&mut self, id: Ident, val: MirandaExpr) {
        if self.name_lookup(&id) {
            self.var_values.insert(id, vec![val]);
        }
    }

    // set function body
    pub fn set_fun_value(&mut self, id: Ident, params: Vec<Ident>, body: Vec<Vec<MirandaExpr>>) {
        match self.function_lookup(&id) {
            Ok(funtype) => {
                let mut fun = UserFunc::new();
                fun.set_params(params.clone());
                fun.set_body(body);
                // extend environment with function params and types
                for (param, param_typ) in params.iter().zip(funtype.1.iter()) {
                    self.extend_var(param.to_string(), param_typ.clone())
                }

                self.fun_values.insert(id, fun);
            }
            Err(e) => println!("Function {} is not defined. Error: {}", id, e),
        }
    }

    // insert a function to the table
    pub fn extend_fn(&mut self, id: Ident, t: Vec<MirandaType>) {
        match self.function_lookup(&id) {
            Ok(f) => {
                println!("Function {} is already defined", f);
                return;
            }
            Err(_) => {
                self.funs_table.insert(id.clone(), FunType::new(id, t));
            }
        }
    }

    // returns false if the name is not found in the symbol table
    // true otherwise
    pub fn name_lookup(&self, id: &Ident) -> bool {
        if self.variable_lookup(id).is_err() && self.function_lookup(id).is_err() {
            return false;
        }
        true
    }

    // checks if variable identifier has specified type
    fn check_var(&self, _id: Ident, _t: MirandaType) -> bool {
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
