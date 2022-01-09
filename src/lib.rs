#[allow(dead_code)]
#[allow(unused)]
extern crate nom;

use wasm_bindgen::prelude::*;

mod eval;
mod reader;
mod type_checker;
mod types;

#[wasm_bindgen]
pub fn parse_and_eval(input: &str, ) -> String {
    let ref mut global_env = types::Env::new();
    match reader::parse_expr(input) {
        Ok((_, expr)) => {
            match eval::eval(&expr, global_env) {
                Some(expr) => expr.to_string(),
                None => "Evaluation error".to_string()
            }
        }
        Err(e) => e.to_string()
    }
}
