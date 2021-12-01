use crate::types::{FunType, Ident, MirandaType, VarTable, VarType, Bindings};

use std::collections::LinkedList;

pub struct SymbolTableEntry {
    env_type: Option<FunType>, // None for the global scope
    vars_table: VarTable,
}

impl SymbolTableEntry {
    // create new function sym table
    pub fn new(fun: FunType) -> Self {
        Self {
            env_type: Some(fun),
            vars_table: VarTable::new(),
        }
    }

    // creates a sym table for a block
    // used to create global sym table
    pub fn block() -> Self {
        Self {
            env_type: None,
            vars_table: VarTable::new(),
        }
    }

    pub fn function_type(&self) -> Option<FunType> {
        if let Some(x) = &self.env_type {
            return Some(x.clone());
        }
        None
    }

    pub fn variable_lookup(&self, id: &Ident) -> Option<VarType> {
        if let Some(x) = self.vars_table.get(id) {
            return Some((*x).clone());
        }
        return None;
    }

    // insert a variable to table
    pub fn extend_var(&mut self, id: &Ident, t: MirandaType) {
        if let Some(v) = self.variable_lookup(id) {
            println!("Variable {} {} already defined", v.0, v.1.to_string());
            return;
        }
        self.vars_table.insert(id.to_string(), VarType::new(id.to_string(), t));
    }

    pub fn name_lookup(&self, id: &Ident) -> Option<Box<dyn Bindings>> {
        if let Some(v) = self.variable_lookup(id) {
	    return Some(Box::new(v))
        }else if let Some(f) = &self.env_type {
	    return Some(Box::new(f.clone()))
	}
	return None        
    }

    // checks if variable identifier has specified type
    pub fn check_var(&self, id: Ident, t: MirandaType) -> bool {
        if let Some(v) = self.variable_lookup(&id) {
            return v.1 == t;
        }
        return false;
    }

    pub fn check_func(&self, id: Ident, t: Vec<MirandaType>) -> bool {
        if let Some(f) = &self.env_type {
            return f.0 == id && f.1 == t;
        }
        return false;
    }
}

pub struct SymbolTable {
    list: LinkedList<SymbolTableEntry>,
}

impl SymbolTable {
    pub fn new() -> Self {
        let mut list = LinkedList::new();
        list.push_back(SymbolTableEntry::block());
        Self { list }
    }

    // Adds new 'block' or function to end of table
    // inner block
    pub fn insert(&mut self, sym: SymbolTableEntry) {
	self.list.push_back(sym);
    }

    // removes the inner most table entry
    pub fn remove(&mut self) {
        self.list.pop_back();
    }

    // lookup
    pub fn lookup(&self, id: Ident) -> Option<Box<dyn Bindings>> {
        for sym_entry in self.list.iter().rev() {
            if let Some(b) = sym_entry.name_lookup(&id) {
                return Some(b)
            }
        }
        return None;
    }
}

mod tests {}
