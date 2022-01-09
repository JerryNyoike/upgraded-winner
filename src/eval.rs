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

// fn eval_if_predicate(pred: Vec<MirandaExpr>, env: Env) -> MirandaExpr {
//     match pred {
//         MirandaExpr::MirandaBuiltInExpr(op1, operator, op2) => match operator {
//             MirandaExpr::MirandaBuiltIn(BuiltIn::GreaterThan) => match op1 {
//                 MirandaExpr::MirandaIdentifier(iden) => {
//                     let val = eval(&iden, env);
//                     match op2 {
//                         MirandaExpr::MirandaIdentifier(iden2) => {
//                             let val2 = eval(&iden2, env);
//                             MirandaExpr::MirandaBoolean(val > val2)
//                         }
//                         _ => panic!("Can only compare values of the same type"),
//                     }
//                 }
//                 MirandaExpr::MirandaInt(num1) => match op2 {
//                     MirandaExpr::MirandaInt(num2) => MirandaExpr::MirandaBoolean(num1 > num2),
//                     _ => panic!("Can only compare values of the same type"),
//                 },
//                 MirandaExpr::MirandaFloat(float1) => match op2 {
//                     MirandaExpr::MirandaFloat(float2) => {
//                         MirandaExpr::MirandaBoolean(float1 > float2)
//                     }
//                 },
//             },
//             MirandaExpr::MirandaBuiltIn(BuiltIn::LessThan) => match op1 {
//                 MirandaExpr::MirandaIdentifier(iden) => todo!(),
//                 MirandaExpr::MirandaInt(num1) => todo!(),
//                 MirandaExpr::MirandaFloat(float1) => todo!(),
//             },
//             _ => unreachable!(),
//         },
//     }
// }

pub fn eval(expr: MirandaExpr, env: &mut Env) -> Option<MirandaExpr> {
    use crate::types::{BuiltIn::*, MirandaExpr::*};

    match expr {
        MirandaExpr::MirandaInt(num) => Some(MirandaExpr::MirandaInt(num)),
        MirandaExpr::MirandaChar(ch) => Some(MirandaExpr::MirandaChar(ch)),
        MirandaExpr::MirandaBoolean(boolean) => Some(MirandaExpr::MirandaBoolean(boolean)),
        MirandaExpr::MirandaString(string) => Some(MirandaExpr::MirandaString(string)),
        MirandaExpr::MirandaFloat(float) => Some(MirandaExpr::MirandaFloat(float)),
        MirandaExpr::MirandaList(list) => Some(MirandaExpr::MirandaList(list)),
        // MirandaExpr::MirandaIf(pred) => eval_if_predicate(*pred, env),
        MirandaBuiltInExpr(built_in_expr) => {
            // TODO support use of bindings in the expression
            let val1 = eval(built_in_expr[0].clone(), env);
            let builtin_op = &built_in_expr[1];
            let val2 = eval(built_in_expr[2].clone(), env);

            match builtin_op {
                MirandaBuiltIn(Plus) => match &val1 {
                    Some(MirandaInt(num1)) => {
                        if let Some(MirandaInt(num2)) = val2 {
                            Some(MirandaInt(num1 + num2))
                        } else {
                            println!(
                                "Addition is not defined over types: {:#?} and {:#?}",
                                val1, val2
                            );
                            None
                        }
                    }
                    Some(MirandaList(ls1)) => {
                        if let Some(MirandaList(ls2)) = val2 {
                            let ls1_p = ls1.clone();
                            let ls2_p = ls2.clone();
                            let mut new_ls = vec![];

                            for val in ls1 {
                                new_ls.push(val.clone());
                            }

                            for val in ls2.iter() {
                                new_ls.push(val.clone());
                            }
                            Some(MirandaList(new_ls.to_vec()))
                        } else {
                            println!("Cannot combine a list with the type: {:#?}", val2);
                            None
                        }
                    }
                    _ => {
                        println!(
                            "Addition not defined over the types: {:#?}, {:#?}",
                            val1, val2
                        );
                        None
                    }
                },
                MirandaBuiltIn(Minus) => match val1 {
                    Some(MirandaInt(num1)) => {
                        if let Some(MirandaInt(num2)) = val2 {
                            Some(MirandaInt(num1 - num2))
                        } else {
                            println!(
                                "Subtraction is not defined over types: {:#?} and {:#?}",
                                val1, val2
                            );
                            None
                        }
                    }
                    _ => {
                        println!(
                            "Equality is not defined over types: {:#?} and {:#?}",
                            val1, val2
                        );
                        None
                    }
                },
                MirandaBuiltIn(Times) => match val1 {
                    Some(MirandaInt(num1)) => {
                        if let Some(MirandaInt(num2)) = val2 {
                            Some(MirandaInt(num1 * num2))
                        } else {
                            println!(
                                "Multiplication is not defined over types: {:#?} and {:#?}",
                                val1, val2
                            );
                            None
                        }
                    }
                    _ => {
                        println!(
                            "Equality is not defined over types: {:#?} and {:#?}",
                            val1, val2
                        );
                        None
                    }
                },
                MirandaBuiltIn(Divide) => match val1 {
                    Some(MirandaInt(num1)) => {
                        if let Some(MirandaInt(num2)) = val2 {
                            Some(MirandaInt(num1 / num2))
                        } else {
                            println!(
                                "Division is not defined over types: {:#?} and {:#?}",
                                val1, val2
                            );
                            None
                        }
                    }
                    _ => {
                        println!(
                            "Equality is not defined over types: {:#?} and {:#?}",
                            val1, val2
                        );
                        None
                    }
                },
                MirandaBuiltIn(Equal) => match val1 {
                    Some(MirandaBoolean(bool1)) => {
                        if let Some(MirandaBoolean(bool2)) = val2 {
                            Some(MirandaBoolean(bool1 == bool2))
                        } else {
                            println!(
                                "Equality is not defined over types: {:#?} and {:#?}",
                                val1, val2
                            );
                            None
                        }
                    }
                    Some(MirandaInt(num1)) => {
                        if let Some(MirandaInt(num2)) = val2 {
                            Some(MirandaBoolean(num1 == num2))
                        } else {
                            println!(
                                "Equality is not defined over types: {:#?} and {:#?}",
                                val1, val2
                            );
                            None
                        }
                    }
                    Some(MirandaChar(ch1)) => {
                        if let Some(MirandaChar(ch2)) = val2 {
                            Some(MirandaBoolean(ch1 == ch2))
                        } else {
                            println!(
                                "Equality is not defined over types: {:#?} and {:#?}",
                                val1, val2
                            );
                            None
                        }
                    }
                    Some(MirandaString(ref str1)) => {
                        if let Some(MirandaString(str2)) = val2 {
                            Some(MirandaBoolean(*str1 == str2))
                        } else {
                            println!(
                                "Equality is not defined over types: {:#?} and {:#?}",
                                val1, val2
                            );
                            None
                        }
                    }
                    _ => {
                        println!(
                            "Equality is not defined over types: {:#?} and {:#?}",
                            val1, val2
                        );
                        None
                    }
                },
                MirandaBuiltIn(Mod) => match val1 {
                    Some(MirandaInt(num1)) => {
                        if let Some(MirandaInt(num2)) = val2 {
                            Some(MirandaInt(num1 % num2))
                        } else {
                            println!(
                                "Remainder is not defined over types: {:#?} and {:#?}",
                                val1, val2
                            );
                            None
                        }
                    }
                    _ => {
                        println!(
                            "Equality is not defined over types: {:#?} and {:#?}",
                            val1, val2
                        );
                        None
                    }
                },
                MirandaBuiltIn(GreaterThan) => match val1 {
                    Some(MirandaInt(num1)) => {
                        if let Some(MirandaInt(num2)) = val2 {
                            Some(MirandaBoolean(num1 > num2))
                        } else {
                            println!(
                                "Greater than is not defined over types: {:#?} and {:#?}",
                                val1, val2
                            );
                            None
                        }
                    }
                    _ => {
                        println!(
                            "Equality is not defined over types: {:#?} and {:#?}",
                            val1, val2
                        );
                        None
                    }
                },
                MirandaBuiltIn(LessThan) => match val1 {
                    Some(MirandaInt(num1)) => {
                        if let Some(MirandaInt(num2)) = val2 {
                            Some(MirandaBoolean(num1 < num2))
                        } else {
                            println!(
                                "Less than is not defined over types: {:#?} and {:#?}",
                                val1, val2
                            );
                            None
                        }
                    }
                    _ => {
                        println!(
                            "Less than is not defined over types: {:#?} and {:#?}",
                            val1, val2
                        );
                        None
                    }
                },
                _ => unreachable!(),
            }
        }
        MirandaExpr::MirandaIf(pred) => {
            let pred_val = eval(*pred, env);
            if let Some(val) = pred_val {
                Some(val)
            } else {
                None
            }
        }
        MirandaBindingDeclaration(vartype) => {
            // check if the variable already exists, alert user you cannot redefine but continue
            // execution.
            env.extend_var(vartype.0, vartype.1);
            Some(MirandaBoolean(true))
        }
        MirandaFunctionDeclaration(funtype) => {
            env.extend_fn(funtype.0, funtype.1);
            Some(MirandaBoolean(true))
        }
        MirandaBindingDefinition(ref ident, ref val) => {
            //check that the binding type has been declared
            match env.variable_lookup(&ident) {
                Ok(vartype) => {
                    let expr_type = check(&val, env);
                    match expr_type {
                        Ok(typ) => {
                            if typ == vartype.1 {
                                // same type as declared
                                env.set_var_value(ident.to_string(), *val.clone());
                                Some(MirandaBoolean(true))
                            } else {
                                None
                            }
                        }
                        Err(e) => {
                            println!("Type error: {}", e);
                            None
                        }
                    }
                }
                Err(e) => {
                    println!(
                        "Variable {} type must be declared before use. Error: {}",
                        ident, e
                    );
                    None
                }
            }
        }
        MirandaFunctionDefinition(ident, params, body) => {
            // check that function type is declared
            match env.function_lookup(&ident) {
                Ok(funtype) => {
                    // function exists,
                    // check the body type
                    for (pos, opt) in body[0].iter().enumerate() {
                        match check(opt, env) {
                            Ok(typ) => {
                                println!("Got here!");
                                println!("{:#?}", typ);
                                println!("{:#?}", funtype.1[pos]);
                                println!("{:#?}", opt);
                                if typ == funtype.1[pos] {
                                    continue;
                                } else {
                                    println!("Type Error. {}", TypeError::Mismatch);
                                    return None;
                                }
                            }
                            Err(e) => {
                                println!("Type Error. {}", e);
                                return None;
                            }
                        }
                    }
                    // all types match create an entry in fun_values for the function
                    env.set_fun_value(ident, params, body);
                    Some(MirandaBoolean(true))
                }
                Err(e) => {
                    println!("Function not defined. Error: {}", e);
                    None
                }
            }
        }
        _ => panic!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{BuiltIn::*, Env, MirandaExpr::*};

    #[test]
    fn eval_test() {
        let ref mut test_env = Env::new();

        let list_val = eval(MirandaList(vec![MirandaInt(1), MirandaInt(2)]), test_env);
        let int_val = eval(MirandaInt(1), test_env);
        let float_val = eval(MirandaFloat(0.98), test_env);
        let char_val = eval(MirandaChar('b'), test_env);
        let string_val = eval(MirandaString("\"hello, world\"".to_string()), test_env);
        let bool_val = eval(MirandaBoolean(true), test_env);

        let add_val = eval(
            MirandaBuiltInExpr(vec![
                MirandaInt(1),
                MirandaBuiltIn(BuiltIn::Plus),
                MirandaInt(1),
                MirandaBuiltIn(BuiltIn::Plus),
                MirandaInt(3),
            ]),
            test_env,
        );
        let append_val = eval(
            MirandaBuiltInExpr(vec![
                MirandaList(vec![MirandaInt(1), MirandaInt(2)]),
                MirandaBuiltIn(BuiltIn::Plus),
                MirandaList(vec![MirandaInt(3), MirandaInt(4)]),
            ]),
            test_env,
        );
        let bool_v = eval(
            MirandaBuiltInExpr(vec![
                MirandaInt(1),
                MirandaBuiltIn(BuiltIn::GreaterThan),
                MirandaInt(2),
            ]),
            test_env,
        );

        let if_v = eval(
            MirandaIf(Box::new(MirandaBuiltInExpr(vec![
                MirandaInt(1),
                MirandaBuiltIn(BuiltIn::Equal),
                MirandaInt(5),
            ]))),
            test_env,
        );

        let age_dec = eval(
            MirandaBindingDeclaration(VarType("age".to_string(), MirandaType::Int)),
            test_env,
        );

        let ls_dec = eval(
            MirandaBindingDeclaration(VarType(
                "stus".to_string(),
                MirandaType::List(Box::new(MirandaType::Int)),
            )),
            test_env,
        );

        let fun_dec = eval(
            MirandaFunctionDeclaration(FunType(
                "add".to_string(),
                vec![MirandaType::Int, MirandaType::Int, MirandaType::Int],
            )),
            test_env,
        );

        let var_def = eval(
            MirandaBindingDefinition("age".to_string(), Box::new(MirandaInt(23))),
            test_env,
        );
        let ls_def = eval(
            MirandaBindingDefinition(
                "stus".to_string(),
                Box::new(MirandaList(vec![
                    MirandaInt(1),
                    MirandaInt(2),
                    MirandaInt(3),
                ])),
            ),
            test_env,
        );

        let fun_def = eval(
            MirandaFunctionDefinition(
                "add".to_string(),
                vec!["a".to_string(), "b".to_string()],
                vec![vec![MirandaBuiltInExpr(vec![
                    MirandaIdentifier("a".to_string()),
                    MirandaBuiltIn(BuiltIn::Plus),
                    MirandaIdentifier("b".to_string()),
                ])]],
            ),
            test_env,
        );

        assert_eq!(
            list_val,
            Some(MirandaExpr::MirandaList(vec![
                MirandaExpr::MirandaInt(1),
                MirandaExpr::MirandaInt(2)
            ]))
        );
        assert_eq!(int_val, Some(MirandaExpr::MirandaInt(1)));
        assert_eq!(char_val, Some(MirandaExpr::MirandaChar('b')));
        assert_eq!(bool_val, Some(MirandaExpr::MirandaBoolean(true)));
        // assert_eq!(float_val, Some(MirandaExpr::MirandaFloat(0.98)));
        assert_eq!(add_val, Some(MirandaExpr::MirandaInt(2)));
        assert_eq!(
            append_val,
            Some(MirandaExpr::MirandaList(vec![
                MirandaExpr::MirandaInt(1),
                MirandaExpr::MirandaInt(2),
                MirandaExpr::MirandaInt(3),
                MirandaExpr::MirandaInt(4)
            ]))
        );
        assert_eq!(bool_v, Some(MirandaExpr::MirandaBoolean(false)));
        assert_eq!(if_v, Some(MirandaBoolean(false)));
        assert_eq!(age_dec, Some(MirandaBoolean(true)));
        assert_eq!(ls_dec, Some(MirandaBoolean(true)));
        assert_eq!(var_def, Some(MirandaBoolean(true)));
        assert_eq!(ls_def, Some(MirandaBoolean(true)));
        assert_eq!(fun_def, Some(MirandaBoolean(true)));
    }
}
