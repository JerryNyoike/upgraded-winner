use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alphanumeric1, char, digit1, i32, one_of},
    combinator::recognize,
    combinator::{map, map_res, value},
    error::VerboseError,
    multi::many0,
    multi::separated_list0,
    sequence::{delimited, pair, preceded},
    IResult,
};

use crate::types::*;

fn parens(input: &str) -> IResult<&str, &str> {
    delimited(char('('), is_not(")"), char(')'))(input)
}

fn parse_integer(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    let (rest_input, matched) = match i32(input) {
        Ok(x) => x,
        Err(e) => return Err(e),
    };

    Ok((rest_input, MirandaExpr::MirandaNum(matched)))
}

fn list(input: &str) -> IResult<&str, Vec<MirandaExpr>, VerboseError<&str>> {
    // we can have a list of numbers, characters or strings
    delimited(
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
            digit_str.parse::<i32>().map(MirandaExpr::MirandaNum)
        }),
        map(preceded(tag("-"), digit1), |digit_str: &str| {
            MirandaExpr::MirandaNum(-1 * digit_str.parse::<i32>().unwrap())
        }),
    ))(input)
}

fn parse_builtin_op(input: &str) -> IResult<&str, BuiltIn, VerboseError<&str>> {
    let (input, t) = one_of("+-*/=%")(input)?;
    Ok((
        input,
        match t {
            '+' => BuiltIn::Plus,
            '-' => BuiltIn::Minus,
            '*' => BuiltIn::Times,
            '/' => BuiltIn::Divide,
            '=' => BuiltIn::Equal,
            '%' => BuiltIn::Mod,
            _ => unreachable!(),
        },
    ))
}

fn parse_if(input: &str) -> IResult<&str, &str> {
    // get condition
    // let (input, cond) = match delimited(tag("if"), is_not("then"), tag("then"))(input) {
    //     Ok(x) => x,
    //     Err(e) => return Err(e),
    // };

    // let (input, cond) = match

    // Ok((input, ""))
    todo!()
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

// fn parse_keyword(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
//     let keywords: Vec<&str> = vec!["if", "where", "otherwise", "type"];

//     for keyword in keywords {
//         match tag(keyword)(input) {
//             Ok((i, _)) => {
//                 let kw = match keyword {
//                     "if" => Keyword::If,
//                     "where" => Keyword::Where,
//                     "otherwise" => Keyword::Otherwise,
//                     "type" => Keyword::Type,
//                     _ => unreachable!(),
//                 };
//                 return Ok((i, MirandaExpr::MirandaKeyword(kw)));
//             },
//             Err(e) => continue,
//         };
//     }

//     let e = nom::Err::Incomplete(nom::Needed::new(0));
//     return Err(e);
// }

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
        Ok((r_input, matched)) => (r_input, matched),
        Err(e) => return Err(e),
    };

    Ok((
        rest_input,
        MirandaExpr::MirandaIdentifier(matched.to_string()),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::MirandaExpr::*;

    #[test]
    fn parens_test() {
        assert_eq!(parens("(hello sir)"), Ok(("", "hello sir")));
    }

    #[test]
    fn list_test() {
        let val = match list("[1,2,3]") {
            Ok((_, matched)) => {
                let mut vals = vec![];
                for m in matched {
                    match m {
                        MirandaNum(n) => vals.push(n),
                        _ => panic!("Not a list of integers"),
                    }
                }
                vals
            }
            Err(_) => panic!("Failed to parse list."),
        };

        assert_eq!(val, vec![1, 2, 3])
    }

    #[test]
    fn boolean_test() {
        let value = match parse_bool("True") {
            Ok((_, mirand)) => match mirand {
                MirandaBoolean(x) => x,
                _ => false,
            },
            Err(_) => false,
        };

        assert_eq!(value, true);

        let value = match parse_bool("False") {
            Ok((_, mirand)) => match mirand {
                MirandaBoolean(x) => x,
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
                MirandaNum(x) => x,
                _ => panic!("this test has failed...like you"),
            },
            Err(_) => panic!("what did you expect"),
        };

        assert_eq!(value, 123);
        let value = match parse_num("-123") {
            Ok((_, mirand)) => match mirand {
                MirandaNum(x) => x,
                _ => panic!("this test has failed...like you"),
            },
            Err(_) => panic!("what did you expect"),
        };

        assert_eq!(value, -123);
    }

    #[test]
    fn builtin_test() -> Result<(), String> {
        let inp = "+-/*=%";
        let (inp, bt) = match parse_builtin_op(inp) {
            Ok((i, b)) => match b {
                BuiltIn::Plus => (i, b),
                _ => return Err("aren't you tired of failure".to_string()),
            },
            Err(_) => panic!("aren't you tired of failure"),
        };

        let (inp, bt) = match parse_builtin_op(inp) {
            Ok((i, b)) => match b {
                BuiltIn::Minus => (i, b),
                _ => return Err("aren't you tired of failure".to_string()),
            },
            Err(_) => panic!("aren't you tired of failure"),
        };

        let (inp, bt) = match parse_builtin_op(inp) {
            Ok((i, b)) => match b {
                BuiltIn::Divide => (i, b),
                _ => return Err("aren't you tired of failure".to_string()),
            },
            Err(_) => panic!("aren't you tired of failure"),
        };

        let (inp, bt) = match parse_builtin_op(inp) {
            Ok((i, b)) => match b {
                BuiltIn::Times => (i, b),
                _ => return Err("aren't you tired of failure".to_string()),
            },
            Err(_) => panic!("aren't you tired of failure"),
        };

        let (inp, bt) = match parse_builtin_op(inp) {
            Ok((i, b)) => match b {
                BuiltIn::Equal => (i, b),
                _ => return Err("aren't you tired of failure".to_string()),
            },
            Err(_) => panic!("aren't you tired of failure"),
        };

        let (inp, bt) = match parse_builtin_op(inp) {
            Ok((i, b)) => match b {
                BuiltIn::Mod => (i, b),
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
                MirandaChar(x) => x,
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
                MirandaString(x) => x,
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
                MirandaIdentifier(x) => x,
                _ => panic!("Error"),
            },
            Err(_) => panic!("Could not parse identifier"),
        };

        assert_eq!(value, "hello123_world".to_string());

        let value = match parse_identifier("hello123_world'") {
            Ok((_, matched)) => match matched {
                MirandaIdentifier(x) => x,
                _ => panic!("Error"),
            },
            Err(_) => panic!("Could not parse identifier"),
        };

        assert_eq!(value, "hello123_world'".to_string());
    }

    #[test]
    fn parse_integer_test() {
        let val = match parse_integer("12345") {
            Ok((_, matched)) => match matched {
                MirandaNum(n) => n,
                _ => panic!("Not a number"),
            },
            Err(_) => panic!("Failed"),
        };

        assert_eq!(val, 12345);

        let val = match parse_integer("-12345") {
            Ok((_, matched)) => match matched {
                MirandaNum(n) => n,
                _ => panic!("Not a number"),
            },
            Err(_) => panic!("Failed"),
        };

        assert_eq!(val, -12345);

    }
}
