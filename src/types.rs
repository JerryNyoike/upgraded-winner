use crate::type_checker::*;

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
    fn extend_var(&mut self, id: Ident, t: MirandaType) {
        if let None = self.vars_table.get(&id) {
            // Type Ident already defined
            println!("Variable {} {} already defined", t.to_string(), id);
            return;
        }
        self.vars_table.insert(id.clone(), VarType::new(id, t));
    }

    fn extend_fn(&mut self, id: Ident, t: Vec<MirandaType>) {
        if let None = self.funs_table.get(&id) {
            // Type Ident already defined
            let fun = FunType::new(id, t);
            println!("{}", fun);
            return;
        }
        self.funs_table.insert(id.clone(), FunType::new(id, t));
    }

    fn check(&self, t: MirandaType) {}
}

mod tests {}
