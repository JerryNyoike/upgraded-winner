use crate::types::{FunType, Ident, MirandaType, VarTable, VarType};

pub struct SymbolTableEntry {
    env_type: Option<FunType>, // None for the global scope
    vars_table: VarTable,
}

impl SymbolTableEntry {
    pub fn new(fun: FunType) -> Self {
        Self {
            env_type: Some(fun),
            vars_table: VarTable::new(),
        }
    }

    pub fn global() -> Self {
        Self {
            env_type: None,
            vars_table: VarTable::new(),
        }
    }

    pub fn function_type(&self, identifier: &Ident) -> Option<FunType> {
        if let Some(x) = &self.env_type {
            return Some(x.clone());
        }
        None
    }

    pub fn variable_lookup(&self, identifier: &Ident) -> Option<VarType> {
        if let Some(x) = self.vars_table.get(identifier) {
            return Some((*x).clone());
        }
        return None;
    }

    // insert a variable to table
    pub fn extend_var(&mut self, id: Ident, t: MirandaType) {
        if let Some(v) = self.variable_lookup(&id) {
            println!("Variable {} {} already defined", v.0, v.1.to_string());
            return;
        }
        self.vars_table.insert(id.clone(), VarType::new(id, t));
    }

    // returns false if the name is not found in the symbol table
    // true otherwise
    pub fn name_lookup(&self, id: Ident) -> bool {
        if let None = self.variable_lookup(&id) {
            return false;
        }
        true
    }

    // checks if variable identifier has specified type
    pub fn check_var(&self, id: Ident, t: MirandaType) -> bool {
        // if let Some(v) = variable_lookup()
        false
    }
}

mod tests {}
