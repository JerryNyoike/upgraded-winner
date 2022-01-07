use crate::reader::parse_expr;
use crate::types::*;
use std::any::Any;

// fn analyze_self_evaluating(expr: MirandaExpr) -> impl Fn {
//     move |env: Env| match expr {
//         _ => unreachable!(), // panic!("Error! Not a literal"),
//     }
// }

// fn analyze_binding(expr: MirandaExpr) -> Fn(Env) {
//     move |env: Env| match expr {
//         MirandaExpr::MirandaIdentifier(ident) => {
//             // lookup the variable in the function table and the variables table
//             if env.name_lookup(ident) {
//                 if let Some(var) = env.binding_value(ident) {
//                     let val = eval(var);
//                     val
//                 } else if let Some(fun) = env.function_body() {
//                     let val = eval(fun);
//                     val
//                 }
//             }
//             None
//         }
//         _ => unreachable!(),
//     }
// }

// fn analyze_if(expr: MirandaExpr) -> impl Fn {
//     move |env: Env| match expr {
//         MirandaExpr::MirandaIf(expr1, op, expr2) => {
//             let val1 = eval(expr1, env);
//             let val2 = eval(expr2, env);
//             match op {
//                 MirandaExpr::MirandaBuiltIn(BuiltIn::GreaterThan) => val1 > val2,
//                 MirandaExpr::MirandaBuiltIn(BuiltIn::LessThan) => val1 < val2,
//                 _ => panic!("The if predicate must use a boolean operator"),
//             }
//         }
//         _ => unreachable!(),
//     }
// }

// fn analyze_binding_declaration(expr: MirandaExpr) -> impl Fn {
//     move |env: Env| match expr {
//         MirandaExpr::MirandaBindingDeclaration((ident, typ)) => env.extend_var(ident, typ),
//         _ => unreachable!(),
//     }
// }

// fn analyze_function_declaration(expr: MirandaExpr) -> impl Fn {
//     move |env: Env| match expr {
//         MirandaExpr::MirandaFunctionDeclaration((ident, param_types)) => {
//             env.extend_fn(ident, param_types)
//         }
//         _ => unreachable!(),
//     }
// }

// fn analyze_binding_definition(expr: MirandaExpr) -> impl Fn {
//     move |env| match expr {
//         MirandaExpr::MirandaBindingDefinition(ident, expr1) => {
//             let val = eval(*expr1, env);
//             let typ = env.variable_lookup(ident);
//         }
//     }
// }

// fn analyze_function_definition(expr: MirandaExpr) -> impl Fn {
//     move |env: Env| match expr {
//         MirandaExpr::MirandaBindingDefinition(ident, body) => {
//             todo!()
//         }
//         _ => unreachable!(),
//     }
// }

fn eval_if_predicate(pred: Vec<MirandaExpr>, env: Env) -> MirandaExpr {
    match pred {
        MirandaExpr::MirandaBuiltInExpr(op1, operator, op2) => match operator {
            MirandaExpr::MirandaBuiltIn(BuiltIn::GreaterThan) => match op1 {
                MirandaExpr::MirandaIdentifier(iden) => {
                    let val = eval(&iden, env);
                    match op2 {
                        MirandaExpr::MirandaIdentifier(iden2) => {
                            let val2 = eval(&iden2, env);
                            MirandaExpr::MirandaBoolean(val > val2)
                        }
                        _ => panic!("Can only compare values of the same type"),
                    }
                }
                MirandaExpr::MirandaInt(num1) => match op2 {
                    MirandaExpr::MirandaInt(num2) => MirandaExpr::MirandaBoolean(num1 > num2),
                    _ => panic!("Can only compare values of the same type"),
                },
                MirandaExpr::MirandaFloat(float1) => match op2 {
                    MirandaExpr::MirandaFloat(float2) => {
                        MirandaExpr::MirandaBoolean(float1 > float2)
                    }
                },
            },
            MirandaExpr::MirandaBuiltIn(BuiltIn::LessThan) => match op1 {
                MirandaExpr::MirandaIdentifier(iden) => todo!(),
                MirandaExpr::MirandaInt(num1) => todo!(),
                MirandaExpr::MirandaFloat(float1) => todo!(),
            },
            _ => unreachable!(),
        },
    }
}

pub fn eval(input: &str, env: Env) -> MirandaExpr {
    let res = parse_expr(input);
    match res {
        Ok((_, result)) => match result {
            MirandaExpr::MirandaInt(num) => MirandaExpr::MirandaInt(num),
            MirandaExpr::MirandaChar(ch) => MirandaExpr::MirandaChar(ch),
            MirandaExpr::MirandaBoolean(boolean) => MirandaExpr::MirandaBoolean(boolean),
            MirandaExpr::MirandaString(string) => MirandaExpr::MirandaString(string),
            MirandaExpr::MirandaFloat(float) => MirandaExpr::MirandaFloat(float),
            MirandaExpr::MirandaList(list) => MirandaExpr::MirandaList(list),
            MirandaExpr::MirandaIf(pred) => eval_if_predicate(*pred, env),

            _ => panic!(),
        },
        Err(e) => panic!(),
    }
}
