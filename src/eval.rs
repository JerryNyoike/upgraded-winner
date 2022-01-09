use crate::reader::parse_expr;
use crate::types::*;
use std::any::Any;

pub fn eval(expr: &MirandaExpr, env: &mut Env) -> Option<MirandaExpr> {
    use crate::types::{BuiltIn::*, MirandaExpr::*};

    match expr {
        MirandaExpr::MirandaInt(num) => Some(MirandaExpr::MirandaInt(*num)),
        MirandaExpr::MirandaChar(ch) => Some(MirandaExpr::MirandaChar(*ch)),
        MirandaExpr::MirandaBoolean(boolean) => Some(MirandaExpr::MirandaBoolean(*boolean)),
        MirandaExpr::MirandaString(string) => Some(MirandaExpr::MirandaString(string.clone())),
        MirandaExpr::MirandaFloat(float) => Some(MirandaExpr::MirandaFloat(*float)),
        MirandaExpr::MirandaList(list) => Some(MirandaExpr::MirandaList(list.clone())),
        MirandaExpr::MirandaIdentifier(id) => {
            // ************THIS IS WRONG!**********//
            // check that identifier is declared and defined in the environment
            match env.lookup(id) {
                Ok(typ) => {
                    match typ.1 {
                        MirandaType::Fun(typ) => {
                            // get the function body
                            if let Some(fun_bod) = env.function_body(id) {
                                let mut ts = vec![];
                                for t in typ {
                                    match t {
                                        MirandaType::Bool => {
                                            ts.push(MirandaExpr::MirandaBoolean(true))
                                        }
                                        MirandaType::Int => ts.push(MirandaExpr::MirandaInt(1)),
                                        MirandaType::Float => {
                                            ts.push(MirandaExpr::MirandaFloat(0.9))
                                        }
                                        MirandaType::List(_) => {
                                            ts.push(MirandaExpr::MirandaList(vec![]))
                                        }
                                        MirandaType::Char => ts.push(MirandaExpr::MirandaChar('a')),
                                        MirandaType::String => {
                                            ts.push(MirandaExpr::MirandaString("".to_string()))
                                        }
                                        _ => unreachable!(),
                                    };
                                }
                                Some(MirandaExpr::MirandaList(ts))
                            } else {
                                println!(
                                    "Function declared but not defined. Type Error: {}",
                                    TypeError::NotInScope
                                );
                                None
                            }
                        }
                        _ => {
                            // fetch from the vars in the environment
                            println!("%%%%%%%%%");
                            println!("{:#?}", env.get_function_env()[0]);
                            println!("%%%%%%%%%");
                            if let Some(bind) = env.binding_value(id) {
                                Some(bind)
                            } else if let Some(local_bind) = env.get_function_env()[0].binding_value(id) {
                                Some(local_bind)
                            } else {
                                None
                            }
                        }
                    }
                }
                Err(e) => {
                    // check local environment
                    let ref mut l_env = env.get_function_env();
                    if l_env.len() > 0 {
                        println!("::::::::::::::");
                        println!("{:#?}", l_env[0]);  
                        let ref mut fn_env = l_env[0];
                        return eval(expr, fn_env);
                    }
                    println!("Type Error: {}", e);
                    None
                }
            }
        }
        // MirandaExpr::MirandaIf(pred) => eval_if_predicate(*pred, env),
        MirandaBuiltInExpr(built_in_expr) => {
            // TODO support use of bindings in the expression
            let val1 = eval(&built_in_expr[0], env);
            let builtin_op = &built_in_expr[1];
            let val2 = eval(&built_in_expr[2], env);

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
            let pred_val = eval(pred, env);
            if let Some(val) = pred_val {
                Some(val)
            } else {
                None
            }
        }
        MirandaBindingDeclaration(vartype) => {
            // check if the variable already exists, alert user you cannot redefine but continue
            // execution.
            env.extend_var(vartype.0.clone(), vartype.1.clone());
            Some(MirandaBoolean(true))
        }
        MirandaFunctionDeclaration(funtype) => {
            let fun_param_type = match funtype.1.clone() {
                MirandaType::Fun(x) => x,
                _ => return None,
            };
            env.extend_fn(funtype.0.clone(), fun_param_type);
            Some(MirandaBoolean(true))
        }
        MirandaBindingDefinition(ref ident, ref val) => {
            //check that the binding type has been declared
            match env.lookup(&ident) {
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
            match env.lookup(&ident) {
                Ok(_) => {
                    // function exists, add function to the environment
                    env.set_fun_value(ident.clone(), params.clone(), body.clone());
                    Some(MirandaBoolean(true))
                }
                Err(e) => {
                    println!("Function not defined. Error: {}", e);
                    None
                }
            }
        }
        MirandaFunctionCall(ident, args) => {
            // check types of arguments
            let ref mut function_env = Env::new();

            if let MirandaType::Fun(fn_typ) = env.lookup(&ident).unwrap().1 {
                for (pos, arg) in args.iter().enumerate() {
                    match check(arg, env) {
                        Ok(arg_typ) => {
                            if arg_typ != fn_typ[pos] {
                                println!("Type Error: {}", TypeError::Mismatch);
                                println!(
                                    "From in here arg: {}, arg_type: {:#?}, expected: {:#?}",
                                    arg, arg_typ, fn_typ[pos]
                                );
                                return None;
                            } else {
                                if let Some(function_details) = env.get_fun_value(&ident) {
                                    let param_names = function_details.get_params();
                                    // function_env.

                                    for (arg, p_name) in args.iter().zip(param_names) {
                                        // build up local env
                                        match check(arg, env) {
                                            Ok(arg_typ) => {
                                                function_env.extend_var(p_name.clone(), arg_typ);
                                                function_env
                                                    .set_var_value(p_name.clone(), arg.clone());
                                            }
                                            Err(e) => {
                                                println!("Type Error: {}", e);
                                                println!("From number2");
                                            }
                                        };
                                    }

                                    // println!("^^^^^^^^^^");
                                    // println!("{:#?}", function_env.clone());
                                    // println!("^^^^^^^^^^");
                                    env.add_function_env(function_env.clone());

                                    // evaluate the function body
                                    // TODO add support for function guards
                                    let fun_body = function_details.get_body();

                                    // if the function has guards, find the guard that evals to true and evaluate
                                    // its body
                                    // add 1 ""
                                    if fun_body.len() > 1
                                        && fun_body.iter().all(|ref elem| elem.len() == 2)
                                    {
                                        for bod in fun_body {
                                            match eval(&bod[1], env) {
                                                Some(MirandaBoolean(true)) => eval(&bod[0], env),
                                                _ => continue,
                                            };
                                        }
                                    } else {
                                        // body contains no guards, eval the first element of the body list
                                        // println!("&&&&&&&&&&&&&&");
                                        // println!("{:#?}", env);
                                        // println!("&&&&&&&&&&&&&&");
                                        return eval(&fun_body[0][0], env);
                                    }
                                }
                                println!("No function details");
                                return None;
                            }
                        }
                        Err(e) => {
                            println!("Function not defined. Error: {}", e);
                            return None;
                        }
                    };
                }
            }
            return None;
        }
        _ => panic!("{:#?}", expr),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{BuiltIn::*, Env, MirandaExpr::*};

    #[test]
    fn eval_test() {
        let ref mut test_env = Env::new();

        let list_val = eval(&MirandaList(vec![MirandaInt(1), MirandaInt(2)]), test_env);
        let int_val = eval(&MirandaInt(1), test_env);
        let float_val = eval(&MirandaFloat(0.98), test_env);
        let char_val = eval(&MirandaChar('b'), test_env);
        let string_val = eval(&MirandaString("\"hello, world\"".to_string()), test_env);
        let bool_val = eval(&MirandaBoolean(true), test_env);

        let add_val = eval(
            &MirandaBuiltInExpr(vec![
                MirandaInt(1),
                MirandaBuiltIn(BuiltIn::Plus),
                MirandaInt(1),
                MirandaBuiltIn(BuiltIn::Plus),
                MirandaInt(3),
            ]),
            test_env,
        );
        let append_val = eval(
            &MirandaBuiltInExpr(vec![
                MirandaList(vec![MirandaInt(1), MirandaInt(2)]),
                MirandaBuiltIn(BuiltIn::Plus),
                MirandaList(vec![MirandaInt(3), MirandaInt(4)]),
            ]),
            test_env,
        );
        let bool_v = eval(
            &MirandaBuiltInExpr(vec![
                MirandaInt(1),
                MirandaBuiltIn(BuiltIn::GreaterThan),
                MirandaInt(2),
            ]),
            test_env,
        );

        let if_v = eval(
            &MirandaIf(Box::new(MirandaBuiltInExpr(vec![
                MirandaInt(1),
                MirandaBuiltIn(BuiltIn::Equal),
                MirandaInt(5),
            ]))),
            test_env,
        );

        let age_dec = eval(
            &MirandaBindingDeclaration(VarType("age".to_string(), MirandaType::Int)),
            test_env,
        );

        let ls_dec = eval(
            &MirandaBindingDeclaration(VarType(
                "stus".to_string(),
                MirandaType::List(Box::new(MirandaType::Int)),
            )),
            test_env,
        );

        let fun_dec = eval(
            &MirandaFunctionDeclaration(VarType(
                "add".to_string(),
                MirandaType::Fun(vec![MirandaType::Int, MirandaType::Int, MirandaType::Int]),
            )),
            test_env,
        );

        let var_def = eval(
            &MirandaBindingDefinition("age".to_string(), Box::new(MirandaInt(23))),
            test_env,
        );
        let ls_def = eval(
            &MirandaBindingDefinition(
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
            &MirandaFunctionDefinition(
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

        let fun_app = eval(
            &MirandaExpr::MirandaFunctionCall(
                "add".to_string(),
                vec![
                    MirandaExpr::MirandaInt(1),
                    MirandaExpr::MirandaIdentifier("age".to_string()),
                ],
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

        let fun_def = eval(
            &MirandaFunctionDefinition(
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

        let fun_app = eval(
            &MirandaExpr::MirandaFunctionCall(
                "add".to_string(),
                vec![
                    MirandaExpr::MirandaInt(1),
                    MirandaExpr::MirandaIdentifier("age".to_string()),
                ],
            ),
            test_env,
        );

        assert_eq!(fun_app, Some(MirandaInt(24)));
        assert_eq!(fun_def, Some(MirandaBoolean(true)));
    }
}
