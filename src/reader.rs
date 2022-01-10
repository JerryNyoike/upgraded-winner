use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alphanumeric1, char, digit1, i32, multispace0, multispace1, one_of},
    combinator::{all_consuming, map, map_res, peek, recognize, value},
    error::{Error, ErrorKind, ParseError, VerboseError},
    multi::many0,
    multi::{separated_list0, separated_list1},
    number::complete::float,
    sequence::{delimited, pair, preceded, terminated},
    IResult,
};

use std::collections::HashMap;

use crate::types::*;

fn parens(input: &str) -> IResult<&str, &str> {
    delimited(char('('), is_not(")"), char(')'))(input)
}

pub fn parse_integer(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    let (rest_input, matched) = match i32(input) {
        Ok(x) => x,
        Err(e) => return Err(e),
    };

    Ok((rest_input, MirandaExpr::MirandaInt(matched)))
}

fn list(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    // we can have a list of numbers, characters or strings
    let (r_input, matched) = match delimited(
        char('['),
        separated_list0(
            tag(","),
            alt((
                parse_integer,
                parse_string_literal,
                parse_char_literal,
                parse_bool,
            )),
        ),
        char(']'),
    )(input)
    {
        Ok((r, found)) => (r, MirandaExpr::MirandaList(found)),
        Err(e) => return Err(e),
    };

    Ok((r_input, matched))
}

fn comment(input: &str) -> IResult<&str, ()> {
    value((), pair(tag("||"), is_not("\n\r")))(input)
}

fn parse_bool(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    alt((
        map(tag("True"), |_| MirandaExpr::MirandaBoolean(true)),
        map(tag("False"), |_| MirandaExpr::MirandaBoolean(false)),
    ))(input)
}

fn parse_num(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    alt((
        map_res(digit1, |digit_str: &str| {
            digit_str.parse::<i32>().map(MirandaExpr::MirandaInt)
        }),
        map(preceded(tag("-"), digit1), |digit_str: &str| {
            MirandaExpr::MirandaInt(-1 * digit_str.parse::<i32>().unwrap())
        }),
    ))(input)
}

fn parse_builtin_op(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    let (input, t) = match alt((
        tag("+"),
        tag("-"),
        tag("*"),
        tag("/"),
        tag("%"),
        tag("=="),
        tag(">"),
        tag("<")))(input){
        Ok((rest, found)) => (rest, found),
        Err(e) => return Err(e)
    };
    
    Ok((
        input,
        match t {
            "+" => MirandaExpr::MirandaBuiltIn(BuiltIn::Plus),
            "-" => MirandaExpr::MirandaBuiltIn(BuiltIn::Minus),
            "*" => MirandaExpr::MirandaBuiltIn(BuiltIn::Times),
            "/" => MirandaExpr::MirandaBuiltIn(BuiltIn::Divide),
            "==" => MirandaExpr::MirandaBuiltIn(BuiltIn::Equal),
            "%" => MirandaExpr::MirandaBuiltIn(BuiltIn::Mod),
            ">" => MirandaExpr::MirandaBuiltIn(BuiltIn::GreaterThan),
            "<" => MirandaExpr::MirandaBuiltIn(BuiltIn::LessThan),
            _ => unreachable!(),
        },
    ))
}

fn parse_if(_input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    // an if is preceded by a comma and ended by an empty string or a newline character which shows
    // we have moved to the next guard or end of function definition
    let if_combinator = preceded(
        multispace0,
        delimited(
            tag("if"),
            preceded(multispace0, parse_builtin_expr),
            multispace0,
        ),
    );

    let (input, cond) = match parse_keyword(_input.trim()) {
        Ok((_, _)) => {
            // if keyword matched now get the boolean expression
            match preceded(multispace0, if_combinator)(_input.trim()) {
                Ok((rest_input, matched)) => {
                    // matched boolean expression
                    (rest_input, matched)
                }
                Err(e) => return Err(e),
            }
        }
        Err(e) => return Err(e),
    };

    Ok((input, MirandaExpr::MirandaIf(Box::new(cond))))
}

fn parse_char_literal(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    let (input, chr) = match delimited(char('\''), is_not("'"), char('\''))(input) {
        Ok(x) => x,
        Err(e) => return Err(e),
    };

    let c = match chr.chars().nth(0) {
        Some(x) => x,
        None => {
            let e = nom::Err::Incomplete(nom::Needed::new(0));
            return Err(e);
        }
    };

    Ok((input, MirandaExpr::MirandaChar(c)))
}

fn parse_string_literal(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    let (input, chr) = match delimited(char('"'), is_not("\""), char('"'))(input) {
        Ok(x) => x,
        Err(e) => return Err(e),
    };

    Ok((input, MirandaExpr::MirandaString(chr.to_string())))
}

fn parse_keyword(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    let keywords: Vec<&str> = vec!["if", "where", "otherwise", "type"];

    for keyword in keywords {
        let res: Result<(&str, &str), nom::Err<VerboseError<&str>>> = peek(tag(keyword))(input);
        match res {
            Ok((i, _)) => {
                let kw = match keyword {
                    "if" => Keyword::If,
                    "where" => Keyword::Where,
                    "otherwise" => Keyword::Otherwise,
                    "type" => Keyword::Type,
                    _ => unreachable!(),
                };
                return Ok((i, MirandaExpr::MirandaKeyword(kw)));
            }
            Err(_) => continue,
        };
    }

    let e = nom::Err::Incomplete(nom::Needed::new(0));
    return Err(e);
}

// named!(lower<char>, one_of!("abcdefghijklmnopqrstuvwxyz"));

fn lower(input: &str) -> IResult<&str, char, VerboseError<&str>> {
    let l_case_chars = "abcdefghijklmnopqrstuvwxyz";
    let (rest_input, matched) = match one_of(l_case_chars)(input) {
        Ok(x) => x,
        Err(e) => return Err(e),
    };

    Ok((rest_input, matched))
}

fn parse_identifier(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    // identifier should not start with number
    // run lower parser first then run parser that accepts a-zA-Z0-9'
    let (rest_input, matched) = match recognize(pair(
        pair(lower, many0(alt((alphanumeric1, tag("_"))))),
        alt((tag("'"), tag(""))),
    ))(input)
    {
        Ok(value) => value,
        Err(e) => return Err(e),
    };

    Ok((
        rest_input,
        MirandaExpr::MirandaIdentifier(matched.to_string()),
    ))
}

fn parse_builtin_expr(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    // println!("****************");
    // println!("{}***", input);
    // println!("****************");
    let (rest, first_val) = match preceded(
        multispace0,
        alt((parse_integer, parse_float, parse_identifier, list)),
    )(input)
    {
        Ok((r, found)) => (r, found),
        Err(e) => return Err(e),
    };

    let (rest, builtin) = match alt((
        preceded(multispace0, parse_builtin_op),
        parse_builtin_op,
        list,
    ))(rest)
    {
        Ok((r, found)) => (r, found),
        Err(e) => return Err(e),
    };

    let (rest, second_val) = match preceded(
        multispace0,
        alt((parse_integer, parse_float, parse_identifier, list)),
    )(rest)
    {
        Ok((r, found)) => (r, found),
        Err(e) => return Err(e),
    };

    Ok((
        rest,
        MirandaExpr::MirandaBuiltInExpr(vec![first_val, builtin, second_val]),
    ))
}

fn parse_type(input: &str) -> IResult<&str, MirandaType, VerboseError<&str>> {
    // var :: bool
    match alt((
        tag("bool"),
        tag("int"),
        tag("float"),
        tag("char"),
        tag("string"),
        tag("["),
    ))(input)
    {
        Ok((rest, matched)) => match matched {
            "bool" => Ok((rest, MirandaType::Bool)),
            "int" => Ok((rest, MirandaType::Int)),
            "float" => Ok((rest, MirandaType::Float)),
            "char" => Ok((rest, MirandaType::Char)),
            "string" => Ok((rest, MirandaType::String)),
            "[" => {
                let (r_inp, list_type) = match parse_variable_type(rest) {
                    Ok(x) => x,
                    Err(e) => return Err(e),
                };
                Ok((r_inp, MirandaType::List(Box::new(list_type))))
            }
            _ => {
                panic!("Expected type variable.")
            }
        },
        Err(e) => return Err(e),
    }
}

fn parse_variable_type(input: &str) -> IResult<&str, MirandaType, VerboseError<&str>> {
    let (input, matched) = match preceded(
        multispace0,
        preceded(alt((tag("::"), tag(""))), preceded(multispace0, parse_type)),
    )(input)
    {
        Ok((r_inp, found)) => (r_inp, found),
        Err(e) => return Err(e),
    };

    Ok((input, matched))
}

fn parse_variable_definition(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    let (r_inp, identifier) = match parse_identifier(input) {
        Ok(x) => match x {
            (r, MirandaExpr::MirandaIdentifier(iden)) => (r, iden),
            _ => panic!("Error parsing!"),
        },
        Err(e) => return Err(e),
    };

    let (rest_input, value) = match preceded(
        multispace0,
        preceded(
            multispace0,
            preceded(
                tag("="),
                preceded(
                    multispace0,
                    alt((
                        parse_integer,
                        parse_float,
                        parse_bool,
                        parse_char_literal,
                        parse_string_literal,
                    )),
                ),
            ),
        ),
    )(r_inp)
    {
        Ok((r, found)) => (r, found),
        Err(e) => return Err(e),
    };

    Ok((
        rest_input,
        MirandaExpr::MirandaBindingDefinition(identifier, Box::new(value)),
    ))
}

fn parse_variable_declaration(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    // variable definitions involve a tpye declaration and the actual initialization
    // get identifier name
    let (r_inp, identifier) = match parse_identifier(input) {
        Ok(x) => match x {
            (r, MirandaExpr::MirandaIdentifier(iden)) => (r, iden),
            _ => panic!("Error parsing!"),
        },
        Err(e) => return Err(e),
    };

    let (rest, var_type) = match parse_variable_type(r_inp) {
        Ok(x) => x,
        Err(e) => return Err(e),
    };

    Ok((
        rest,
        MirandaExpr::MirandaBindingDeclaration(VarType(identifier, var_type)),
    ))
}

fn parse_param_types(input: &str) -> IResult<&str, Vec<MirandaType>, VerboseError<&str>> {
    // -> int -> [int]
    preceded(
        multispace0,
        preceded(
            delimited(multispace0, tag("->"), multispace0),
            separated_list0(delimited(multispace0, tag("->"), multispace0), parse_type),
        ),
    )(input)
}

fn parse_function_type(input: &str) -> IResult<&str, Vec<MirandaType>, VerboseError<&str>> {
    // add :: int -> int -> int
    // add a b = a + b, if a > b
    //         = b + a, if b > a
    let mut param_types = vec![];
    // a function's type takes the pattern add a b :: int -> int -> int
    let (rest, _) = match parse_variable_type(input) {
        Ok((r, found)) => {
            param_types.push(found.clone());
            (r, found)
        }
        Err(e) => return Err(e),
    };

    let (rest_input, _) = match parse_param_types(rest) {
        Ok((r, types)) => {
            for typ in types.iter() {
                param_types.push(typ.clone())
            }
            (r, types)
        }
        Err(e) => return Err(e),
    };

    Ok((rest_input, param_types))
}

fn parse_function_guard(input: &str) -> IResult<&str, Vec<MirandaExpr>, VerboseError<&str>> {
    let (rest, expr) = match preceded(multispace0, terminated(parse_expr, tag(",")))(input) {
        Ok((r, matched)) => match parse_if(r) {
            Ok((r_inp, found)) => (r_inp, vec![matched, found]),
            Err(e) => return Err(e),
        },
        Err(e) => return Err(e),
    };

    Ok((rest, expr))
}

fn parse_function_declaration(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    // add a b :: int -> int -> int
    // add a b = a + b, if a > b
    //         = b + a, if b > a
    //         where
    //         x = a * a
    let (rest, fun_identifier) = match parse_identifier(input) {
        Ok((r, iden)) => match iden {
            MirandaExpr::MirandaIdentifier(name) => (r, name),
            _ => panic!("Not a Miranda identifier"),
        },
        Err(e) => return Err(e),
    };

    let (rest, param_types) = match parse_function_type(rest) {
        Ok((r, p_types)) => (r, p_types),
        Err(e) => return Err(e),
    };

    let fun_type = VarType(
        fun_identifier.clone(),
        MirandaType::Fun(param_types.clone()),
    );

    Ok((rest, MirandaExpr::MirandaFunctionDeclaration(fun_type)))
}

fn parse_function_definition(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    // add a b = a + b, if a > b
    //         = b + a, if b > a
    //         where
    //         x = a * a
    let (fun, param_identifiers) =
        match preceded(multispace0, separated_list0(multispace1, parse_identifier))(input) {
            Ok((r, idens)) => (r, idens),
            Err(e) => return Err(e),
        };

    let mut identifiers: Vec<Ident> = vec![];

    for id in param_identifiers.into_iter() {
        match id {
            MirandaExpr::MirandaIdentifier(ident) => identifiers.push(ident.to_string()),
            _ => panic!("Invalid identifier"),
        }
    }

    let (rest, function_body) = match preceded(
        multispace1,
        preceded(tag("="), separated_list1(tag("="), parse_function_guard)),
    )(fun)
    {
        Ok((r, matched)) => (r, matched),
        Err(e) => return Err(e),
    };

    Ok((
        rest,
        MirandaExpr::MirandaFunctionDefinition(
            identifiers[0].to_string(),
            identifiers[1..].to_vec(),
            function_body,
        ),
    ))
}

fn parse_function_definition_without_guard(
    input: &str,
) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    // add a b :: int -> int -> int
    // add a b = a + b, if a > b
    //         = b + a, if b > a
    //         where
    //         x = a * a
    //
    // add a b = a + b
    let (fun, param_identifiers) =
        match preceded(multispace0, separated_list0(multispace1, parse_identifier))(input) {
            Ok((r, idens)) => (r, idens),
            Err(e) => return Err(e),
        };

    let mut identifiers: Vec<Ident> = vec![];

    for id in param_identifiers.into_iter() {
        match id {
            MirandaExpr::MirandaIdentifier(ident) => identifiers.push(ident.to_string()),
            _ => panic!("Invalid identifier"),
        }
    }

    let (rest, function_body) = match preceded(
        multispace1,
        preceded(tag("="), delimited(multispace0, parse_expr, multispace0)),
    )(fun)
    {
        Ok((r, matched)) => (r, matched),
        Err(e) => return Err(e),
    };

    Ok((
        rest,
        MirandaExpr::MirandaFunctionDefinition(
            identifiers[0].to_string(),
            identifiers[1..].to_vec(),
            vec![vec![function_body]],
        ),
    ))
}

fn parse_float(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    let (input, value) = match float(input) {
        Ok(val) => val,
        Err(e) => return Err(e),
    };

    Ok((input, MirandaExpr::MirandaFloat(value)))
}

fn parse_function_argument(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    let (rest, matched) = match alt((
        parse_char_literal,
        parse_integer,
        parse_bool,
        parse_float,
        parse_string_literal,
        parse_identifier,
    ))(input)
    {
        Ok((r, found)) => (r, found),
        Err(e) => return Err(e),
    };

    Ok((rest, matched))
}

fn parse_function_application(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    let (args, ident) = match parse_identifier(input) {
        Ok((r, f_ident)) => (r, f_ident),
        Err(e) => return Err(e),
    };

    let fun_ident: String;

    match ident {
        MirandaExpr::MirandaIdentifier(f_ident) => {
            fun_ident = f_ident;
        }
        _ => panic!("Failed to parse function application."),
    }

    let (rest, args) = match (preceded(
        multispace0,
        separated_list0(multispace1, parse_function_argument),
    ))(args)
    {
        Ok((r, matched)) => (r, matched),
        Err(e) => return Err(e),
    };
    Ok((rest, MirandaExpr::MirandaFunctionCall(fun_ident, args)))
}

pub fn parse_expr(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    let (rest_input, matched) = match alt((
        parse_function_declaration,
        parse_function_definition,
        parse_function_definition_without_guard,
        parse_variable_declaration,
        parse_variable_definition,
        parse_function_application,
        parse_builtin_expr,
        parse_bool,
        parse_char_literal,
        parse_string_literal,
        parse_integer,
        parse_float,
        list,
        parse_identifier,
        parse_builtin_op
    ))(input)
    {
        Ok((r, m)) => (r, m),
        Err(e) => return Err(e),
    };

    Ok((rest_input, matched))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{BuiltIn, Keyword, MirandaExpr, MirandaType};

    #[test]
    fn parens_test() {
        assert_eq!(parens("(hello sir)"), Ok(("", "hello sir")));
    }

    #[test]
    fn list_test() {
        let val = match list("[1,2,3]") {
            Ok((_, matched)) => matched,
            Err(_) => panic!("Failed to parse list."),
        };

        assert_eq!(
            val,
            MirandaExpr::MirandaList(vec![
                MirandaExpr::MirandaInt(1),
                MirandaExpr::MirandaInt(2),
                MirandaExpr::MirandaInt(3)
            ])
        )
    }

    #[test]
    fn boolean_test() {
        let value = match parse_bool("True") {
            Ok((_, mirand)) => match mirand {
                MirandaExpr::MirandaBoolean(x) => x,
                _ => false,
            },
            Err(_) => false,
        };

        assert_eq!(value, true);

        let value = match parse_bool("False") {
            Ok((_, mirand)) => match mirand {
                MirandaExpr::MirandaBoolean(x) => x,
                _ => true,
            },
            Err(_) => true,
        };

        assert_eq!(value, false);
    }

    #[test]
    fn comment_test() {
        assert_eq!(comment("|| This is comment"), Ok(("", ())));
    }

    #[test]
    fn num_parse_test() {
        let value = match parse_num("123") {
            Ok((_, mirand)) => match mirand {
                MirandaExpr::MirandaInt(x) => x,
                _ => panic!("this test has failed...like you"),
            },
            Err(_) => panic!("what did you expect"),
        };

        assert_eq!(value, 123);
        let value = match parse_num("-123") {
            Ok((_, mirand)) => match mirand {
                MirandaExpr::MirandaInt(x) => x,
                _ => panic!("this test has failed...like you"),
            },
            Err(_) => panic!("what did you expect"),
        };

        assert_eq!(value, -123);
    }

    #[test]
    fn builtin_test() -> Result<(), String> {
        let inp = "+-/*=%";
        let (inp, _) = match parse_builtin_op(inp) {
            Ok((i, b)) => match b {
                MirandaExpr::MirandaBuiltIn(BuiltIn::Plus) => (i, b),
                _ => return Err("aren't you tired of failure".to_string()),
            },
            Err(_) => panic!("aren't you tired of failure"),
        };

        let (inp, _) = match parse_builtin_op(inp) {
            Ok((i, b)) => match b {
                MirandaExpr::MirandaBuiltIn(BuiltIn::Minus) => (i, b),
                _ => return Err("aren't you tired of failure".to_string()),
            },
            Err(_) => panic!("aren't you tired of failure"),
        };

        let (inp, _) = match parse_builtin_op(inp) {
            Ok((i, b)) => match b {
                MirandaExpr::MirandaBuiltIn(BuiltIn::Divide) => (i, b),
                _ => return Err("aren't you tired of failure".to_string()),
            },
            Err(_) => panic!("aren't you tired of failure"),
        };

        let (inp, _) = match parse_builtin_op(inp) {
            Ok((i, b)) => match b {
                MirandaExpr::MirandaBuiltIn(BuiltIn::Times) => (i, b),
                _ => return Err("aren't you tired of failure".to_string()),
            },
            Err(_) => panic!("aren't you tired of failure"),
        };

        let (inp, _) = match parse_builtin_op(inp) {
            Ok((i, b)) => match b {
                MirandaExpr::MirandaBuiltIn(BuiltIn::Equal) => (i, b),
                _ => return Err("aren't you tired of failure".to_string()),
            },
            Err(_) => panic!("aren't you tired of failure"),
        };

        let (inp, _) = match parse_builtin_op(inp) {
            Ok((i, b)) => match b {
                MirandaExpr::MirandaBuiltIn(BuiltIn::Mod) => (i, b),
                _ => return Err("aren't you tired of failure".to_string()),
            },
            Err(_) => panic!("aren't you tired of failure"),
        };

        Ok(())
    }

    #[test]
    fn char_literal_test() {
        let value = match parse_char_literal("'c'") {
            Ok((_, mirand)) => match mirand {
                MirandaExpr::MirandaChar(x) => x,
                _ => panic!("this test has failed...like you"),
            },
            Err(_) => panic!("what did you expect"),
        };

        assert_eq!(value, 'c');
    }

    #[test]
    fn str_literal_test() {
        let value = match parse_string_literal("\"This is a string\"") {
            Ok((_, mirand)) => match mirand {
                MirandaExpr::MirandaString(x) => x,
                _ => panic!("this test has failed...like you"),
            },
            Err(_) => panic!("what did you expect"),
        };

        assert_eq!(value, "This is a string");
    }

    #[test]
    fn lower_case_test() {
        let value = match lower("abcde") {
            Ok((_, matched)) => matched,
            Err(_) => panic!("Failed to match"),
        };

        assert_eq!(value, 'a');
    }

    #[test]
    fn parse_identifier_test() {
        let value = match parse_identifier("hello123_world") {
            Ok((_, matched)) => match matched {
                MirandaExpr::MirandaIdentifier(x) => x,
                _ => panic!("Error"),
            },
            Err(_) => panic!("Could not parse identifier"),
        };

        let val2 =
            match separated_list0(multispace1, parse_identifier)("a b c :: int -> int -> int") {
                Ok((_, matched)) => matched,
                _ => panic!("Failed"),
            };

        assert_eq!(value, "hello123_world".to_string());

        let value = match parse_identifier("hello123_world'") {
            Ok((_, matched)) => match matched {
                MirandaExpr::MirandaIdentifier(x) => x,
                _ => panic!("Error"),
            },
            Err(_) => panic!("Could not parse identifier"),
        };

        assert_eq!(value, "hello123_world'".to_string());

        assert_eq!(
            val2,
            [
                MirandaExpr::MirandaIdentifier("a".to_string()),
                MirandaExpr::MirandaIdentifier("b".to_string()),
                MirandaExpr::MirandaIdentifier("c".to_string())
            ]
        );
    }

    #[test]
    fn parse_integer_test() {
        let val = match parse_integer("12345") {
            Ok((_, matched)) => match matched {
                MirandaExpr::MirandaInt(n) => n,
                _ => panic!("Not a number"),
            },
            Err(_) => panic!("Failed"),
        };

        assert_eq!(val, 12345);

        let val = match parse_integer("-12345") {
            Ok((_, matched)) => match matched {
                MirandaExpr::MirandaInt(n) => n,
                _ => panic!("Not a number"),
            },
            Err(_) => panic!("Failed"),
        };

        assert_eq!(val, -12345);
    }

    #[test]
    fn parse_keyword_test() {
        let keywords: Vec<&str> = vec!["if", "where", "otherwise", "type"];
        let mut matched_keywords = vec![];

        for kw in keywords {
            let _ = match parse_keyword(kw) {
                Ok((_, matched)) => match matched {
                    MirandaExpr::MirandaKeyword(x) => matched_keywords.push(x),
                    _ => panic!("test failed"),
                },
                Err(_) => panic!("test failed"),
            };
        }

        assert_eq!(Keyword::If, matched_keywords[0]);
        assert_eq!(Keyword::Where, matched_keywords[1]);
        assert_eq!(Keyword::Otherwise, matched_keywords[2]);
        assert_eq!(Keyword::Type, matched_keywords[3]);
    }

    #[test]
    fn parse_if_test() {
        let input = " if a>b";
        let val = match parse_if(input) {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Failed to parse if: {}", e),
        };

        let wspace_input = " if a>b \n";
        let val2 = match parse_if(wspace_input) {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Failed to parse if: {}", e),
        };
        assert_eq!(
            val,
            MirandaExpr::MirandaIf(Box::new(MirandaExpr::MirandaBuiltInExpr(vec![
                MirandaExpr::MirandaIdentifier("a".to_string()),
                MirandaExpr::MirandaBuiltIn(BuiltIn::GreaterThan),
                MirandaExpr::MirandaIdentifier("b".to_string())
            ])))
        );

        assert_eq!(
            val2,
            MirandaExpr::MirandaIf(Box::new(MirandaExpr::MirandaBuiltInExpr(vec![
                MirandaExpr::MirandaIdentifier("a".to_string()),
                MirandaExpr::MirandaBuiltIn(BuiltIn::GreaterThan),
                MirandaExpr::MirandaIdentifier("b".to_string())
            ])))
        );
    }

    #[test]
    fn parse_float_test() {
        let val = match parse_float("11e-1") {
            Ok((_, matched)) => match matched {
                MirandaExpr::MirandaFloat(n) => n,
                _ => panic!("Not a number"),
            },
            Err(_) => panic!("Failed"),
        };

        assert_eq!(val, 1.1);

        let val = match parse_float(".42") {
            Ok((_, matched)) => match matched {
                MirandaExpr::MirandaFloat(n) => n,
                _ => panic!("Not a number"),
            },
            Err(_) => panic!("Failed"),
        };

        assert_eq!(val, 0.42);

        let val = match parse_float("-11e-1") {
            Ok((_, matched)) => match matched {
                MirandaExpr::MirandaFloat(n) => n,
                _ => panic!("Not a number"),
            },
            Err(_) => panic!("Failed"),
        };

        assert_eq!(val, -1.1);
    }

    #[test]
    fn parse_variable_type_test() {
        let val = match parse_variable_type(" :: int") {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Error parsing {}", e),
        };

        let string_val = match parse_variable_type(":: string") {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Error parsing {}", e),
        };

        let list_val = match parse_variable_type(":: [string]") {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Error parsing {}", e),
        };

        assert_eq!(MirandaType::Int, val);
        assert_eq!(MirandaType::String, string_val);
        assert_eq!(MirandaType::List(Box::new(MirandaType::String)), list_val);
    }

    #[test]
    fn parse_variable_declaration_test() {
        let val = match parse_variable_declaration("x :: int") {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Failed to parse var declaration: {}", e),
        };

        assert_eq!(
            val,
            MirandaExpr::MirandaBindingDeclaration(VarType("x".to_string(), MirandaType::Int))
        );
    }

    #[test]
    fn parse_variable_definition_test() {
        let val = match parse_variable_definition("jerry = 1") {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Failed to parse var definition: {}", e),
        };

        assert_eq!(
            val,
            MirandaExpr::MirandaBindingDefinition(
                "jerry".to_string(),
                Box::new(MirandaExpr::MirandaInt(1))
            )
        )
    }

    #[test]
    fn parse_function_type_test() {
        let val = match parse_function_type(" :: int -> int -> [int]") {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Failed to parse function type: {}", e),
        };

        assert_eq!(
            val,
            vec![
                MirandaType::Int,
                MirandaType::Int,
                MirandaType::List(Box::new(MirandaType::Int))
            ]
        )
    }

    #[test]
    fn parse_param_types_test() {
        let val = match parse_param_types(" -> int -> [int] \n") {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Failed {}", e),
        };

        assert_eq!(
            val,
            vec![
                MirandaType::Int,
                MirandaType::List(Box::new(MirandaType::Int))
            ]
        )
    }

    #[test]
    fn parse_function_guard_test() {
        let value = match parse_function_guard("15, if a > b") {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Failed to parse: {}", e),
        };

        assert_eq!(
            value,
            vec![
                MirandaExpr::MirandaInt(15),
                MirandaExpr::MirandaIf(Box::new(MirandaExpr::MirandaBuiltInExpr(vec![
                    MirandaExpr::MirandaIdentifier("a".to_string()),
                    MirandaExpr::MirandaBuiltIn(BuiltIn::GreaterThan),
                    MirandaExpr::MirandaIdentifier("b".to_string())
                ])))
            ]
        )
    }

    #[test]
    fn parse_function_declaration_test() {
        let val = match parse_function_declaration("add :: int -> int -> int") {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Failed to parse function declaration: {}", e),
        };

        assert_eq!(
            val,
            MirandaExpr::MirandaFunctionDeclaration(VarType(
                "add".to_string(),
                MirandaType::Fun(vec![MirandaType::Int, MirandaType::Int, MirandaType::Int])
            ))
        );
    }

    #[test]
    fn parse_function_definition_test() {
        let inp = "add a b = 15, if a>b\n = 16, if b>a";

        let value = match parse_function_definition(inp) {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Failed to parse: {}", e),
        };

        assert_eq!(
            value,
            MirandaExpr::MirandaFunctionDefinition(
                "add".to_string(),
                vec!["a".to_string(), "b".to_string()],
                vec![
                    vec![
                        MirandaExpr::MirandaInt(15),
                        MirandaExpr::MirandaIf(Box::new(MirandaExpr::MirandaBuiltInExpr(vec![
                            MirandaExpr::MirandaIdentifier("a".to_string()),
                            MirandaExpr::MirandaBuiltIn(BuiltIn::GreaterThan),
                            MirandaExpr::MirandaIdentifier("b".to_string())
                        ])))
                    ],
                    vec![
                        MirandaExpr::MirandaInt(16),
                        MirandaExpr::MirandaIf(Box::new(MirandaExpr::MirandaBuiltInExpr(vec![
                            MirandaExpr::MirandaIdentifier("b".to_string()),
                            MirandaExpr::MirandaBuiltIn(BuiltIn::GreaterThan),
                            MirandaExpr::MirandaIdentifier("a".to_string())
                        ])))
                    ]
                ]
            )
        );

        let inp2 = "add a b = 25";

        let value2 = match parse_function_definition_without_guard(inp2) {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Failed to parse: {}", e),
        };

        assert_eq!(
            value2,
            MirandaExpr::MirandaFunctionDefinition(
                "add".to_string(),
                vec!["a".to_string(), "b".to_string()],
                vec![vec![MirandaExpr::MirandaInt(25)]]
            )
        );
    }

    #[test]
    fn parse_function_application_test() {
        let input = "add 1 num2";
        let mr = match parse_function_application(input) {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Failed to parse: {}", e),
        };

        assert_eq!(
            mr,
            MirandaExpr::MirandaFunctionCall(
                "add".to_string(),
                vec![
                    MirandaExpr::MirandaInt(1),
                    MirandaExpr::MirandaIdentifier("num2".to_string())
                ]
            )
        );
    }
    #[test]
    fn parse_expr_test() {
        let inp = "inp";

        let inp2 = "add :: int -> int -> int";

        let val = match parse_expr(inp) {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Failed to parse: {}", e),
        };

        let val2 = match parse_expr(inp2) {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Failed to parse: {}", e),
        };

        let val3 = match parse_expr("1 + 1") {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Failed to parse: {}", e),
        };

        let val4 = match parse_expr("x :: int") {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Failed to parse: {}", e),
        };

        let val5 = match parse_expr("add 1 1") {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Failed to parse: {}", e),
        };

        assert_eq!(val, MirandaExpr::MirandaIdentifier("inp".to_string()));

        assert_eq!(
            val2,
            MirandaExpr::MirandaFunctionDeclaration(VarType(
                "add".to_string(),
                MirandaType::Fun(vec![MirandaType::Int, MirandaType::Int, MirandaType::Int]),
            ),)
        );

        assert_eq!(
            val3,
            MirandaExpr::MirandaBuiltInExpr(vec![
                MirandaExpr::MirandaInt(1),
                MirandaExpr::MirandaBuiltIn(BuiltIn::Plus),
                MirandaExpr::MirandaInt(1)
            ])
        );

        assert_eq!(
            val4,
            MirandaExpr::MirandaBindingDeclaration(VarType("x".to_string(), MirandaType::Int))
        );

        println!("{:#?}", val5);

        assert_eq!(
            val5,
            MirandaExpr::MirandaFunctionApplication(
                "add".to_string(),
                vec![MirandaExpr::MirandaInt(1), MirandaExpr::MirandaInt(1)]
            )
        )
    }

    #[test]
    fn parse_builtin_expr_test() {
        let val = match parse_builtin_expr("1 + 1") {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Failed to parse: {}", e),
        };

        assert_eq!(
            val,
            MirandaExpr::MirandaBuiltInExpr(vec![
                MirandaExpr::MirandaInt(1),
                MirandaExpr::MirandaBuiltIn(BuiltIn::Plus),
                MirandaExpr::MirandaInt(1),
            ])
        );
    }
}
