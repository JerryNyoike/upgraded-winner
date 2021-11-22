use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alphanumeric1, char, digit1, i32, multispace0, multispace1, one_of},
    combinator::recognize,
    combinator::{map, map_res, peek, value},
    error::VerboseError,
    multi::many0,
    multi::separated_list0,
    number::complete::float,
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

    Ok((rest_input, MirandaExpr::MirandaInt(matched)))
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
            digit_str.parse::<i32>().map(MirandaExpr::MirandaInt)
        }),
        map(preceded(tag("-"), digit1), |digit_str: &str| {
            MirandaExpr::MirandaInt(-1 * digit_str.parse::<i32>().unwrap())
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

fn parse_if(_input: &str) -> IResult<&str, (MirandaExpr, &str), VerboseError<&str>> {
    // an if is preceded by a comma and ended by an empty string or a newline character which shows
    // we have moved to the next guard or end of function definition
    let bool_combinator = preceded(multispace1, alt((is_not("\n"), is_not("\r"))));
    let if_combinator = delimited(tag("if"), bool_combinator, alt((tag("\n"), tag(""))));

    let (input, if_stmt, cond) = match parse_keyword(_input.trim()) {
        Ok((_, if_expr)) => {
            // if keyword matched now get the boolean expression
            // TODO replace alt parser with a parser for a boolean expression
            match preceded(multispace0, if_combinator)(_input.trim()) {
                Ok((rest_input, matched)) => {
                    // matched boolean expression
                    (rest_input, if_expr, matched)
                }
                Err(e) => return Err(e),
            }
        }
        Err(e) => return Err(e),
    };

    Ok((input, (if_stmt, cond)))
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

fn parse_float(input: &str) -> IResult<&str, MirandaExpr, VerboseError<&str>> {
    let (input, value) = match float(input) {
        Ok(val) => val,
        Err(e) => return Err(e),
    };

    Ok((input, MirandaExpr::MirandaFloat(value)))
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
                        MirandaInt(n) => vals.push(n),
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
                MirandaInt(x) => x,
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
        let (inp, _) = match parse_builtin_op(inp) {
            Ok((i, b)) => match b {
                BuiltIn::Plus => (i, b),
                _ => return Err("aren't you tired of failure".to_string()),
            },
            Err(_) => panic!("aren't you tired of failure"),
        };

        let (inp, _) = match parse_builtin_op(inp) {
            Ok((i, b)) => match b {
                BuiltIn::Minus => (i, b),
                _ => return Err("aren't you tired of failure".to_string()),
            },
            Err(_) => panic!("aren't you tired of failure"),
        };

        let (inp, _) = match parse_builtin_op(inp) {
            Ok((i, b)) => match b {
                BuiltIn::Divide => (i, b),
                _ => return Err("aren't you tired of failure".to_string()),
            },
            Err(_) => panic!("aren't you tired of failure"),
        };

        let (inp, _) = match parse_builtin_op(inp) {
            Ok((i, b)) => match b {
                BuiltIn::Times => (i, b),
                _ => return Err("aren't you tired of failure".to_string()),
            },
            Err(_) => panic!("aren't you tired of failure"),
        };

        let (inp, _) = match parse_builtin_op(inp) {
            Ok((i, b)) => match b {
                BuiltIn::Equal => (i, b),
                _ => return Err("aren't you tired of failure".to_string()),
            },
            Err(_) => panic!("aren't you tired of failure"),
        };

        let (inp, _) = match parse_builtin_op(inp) {
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
                MirandaInt(n) => n,
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

    #[test]
    fn parse_keyword_test() {
        let keywords: Vec<&str> = vec!["if", "where", "otherwise", "type"];
        let mut matched_keywords = vec![];

        for kw in keywords {
            let _ = match parse_keyword(kw) {
                Ok((_, matched)) => match matched {
                    MirandaKeyword(x) => matched_keywords.push(x),
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
        let input = "if a>b";
        let val = match parse_if(input) {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Failed to parse if: {}", e),
        };

        let wspace_input = " if a>b \n";
        let val2 = match parse_if(wspace_input) {
            Ok((_, matched)) => matched,
            Err(e) => panic!("Failed to parse if: {}", e),
        };
        assert_eq!(val, (MirandaKeyword(Keyword::If), "a>b"));
        assert_eq!(val2, (MirandaKeyword(Keyword::If), "a>b"));
    }

    #[test]
    fn parse_float_test() {
        let val = match parse_float("11e-1") {
            Ok((_, matched)) => match matched {
                MirandaFloat(n) => n,
                _ => panic!("Not a number"),
            },
            Err(_) => panic!("Failed"),
        };

        assert_eq!(val, 1.1);

        let val = match parse_float(".42") {
            Ok((_, matched)) => match matched {
                MirandaFloat(n) => n,
                _ => panic!("Not a number"),
            },
            Err(_) => panic!("Failed"),
        };

        assert_eq!(val, 0.42);

        let val = match parse_float("-11e-1") {
            Ok((_, matched)) => match matched {
                MirandaFloat(n) => n,
                _ => panic!("Not a number"),
            },
            Err(_) => panic!("Failed"),
        };

        assert_eq!(val, -1.1);
    }
}
