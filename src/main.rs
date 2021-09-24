use std::convert::Infallible;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::take;
use nom::bytes::complete::take_while;
use nom::character::complete::alpha0;
use nom::character::complete::line_ending;
use nom::character::complete::space1;
use nom::character::complete::{alpha1, alphanumeric0, multispace0, newline as lf, tab};
use nom::character::complete::{crlf, space0};
use nom::character::is_alphabetic;
use nom::combinator::all_consuming;
use nom::combinator::eof;
use nom::combinator::not;
use nom::combinator::{map, map_res, opt, recognize, value};
use nom::error::{context, ContextError, ErrorKind as NomErrorKind, ParseError};
use nom::multi::many_till;
use nom::multi::{many0, many1};
use nom::number::complete::f64;
use nom::number::complete::float;
use nom::sequence::tuple;
use nom::sequence::{delimited, pair, preceded, terminated};
use nom::Err::Failure;
use nom::Finish;
use nom::{self};

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

mod parser;
use parser::*;

pub type Result<'a, T> = nom::IResult<&'a str, T, BeemoScanError<&'a str>>;

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    Nom(NomErrorKind),
    Context(&'static str),
    Custom(String),
}

#[derive(Debug, PartialEq)]
pub struct BeemoScanError<I> {
    pub errors: Vec<(I, ErrorKind)>,
}

#[derive(Debug, Error)]
pub enum BeemoError {
    #[error("Parse error: {0}.")]
    ParseError(String),
    #[error("Scan error.")]
    ScanError(Vec<(String, ErrorKind)>),
}

impl<I> ParseError<I> for BeemoScanError<I> {
    fn from_error_kind(input: I, kind: NomErrorKind) -> Self {
        Self {
            errors: vec![(input, ErrorKind::Nom(kind))],
        }
    }

    fn append(input: I, kind: NomErrorKind, mut other: Self) -> Self {
        other.errors.push((input, ErrorKind::Nom(kind)));
        other
    }

    fn from_char(input: I, c: char) -> Self {
        Self {
            errors: vec![(input, ErrorKind::Context("char"))],
        }
    }
}

impl<I> BeemoScanError<I> {
    pub fn custom(input: I, msg: String) -> Self {
        Self {
            errors: vec![(input, ErrorKind::Custom(msg))],
        }
    }
}

impl<I> ContextError<I> for BeemoScanError<I> {
    fn add_context(input: I, ctx: &'static str, mut other: Self) -> Self {
        other.errors.push((input, ErrorKind::Context(ctx)));
        other
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    Keyword(String),
    Float(f32),
    Colon,
    Plus,
    Multiply,
    Comma,
    Indent,
    Dedent,
    GreaterThan,
    LessThan,
    OpeningParen,
    ClosingParen,
    Eof,
}

#[derive(Debug)]
struct IndentationCounter {
    current: isize,
}

fn keyword(input: &str) -> Result<Token> {
    map(alt((tag("return"), tag("if"))), |k: &str| {
        Token::Keyword(k.to_string())
    })(input)
}

fn identifier(input: &str) -> Result<Token> {
    match map(recognize(pair(alpha1, alphanumeric0)), |s: &str| {
        Token::Identifier(s.to_string())
    })(input)
    {
        Ok((rest, t)) => Ok((rest, t)),
        Err(nom::Err::Error(_)) => Err(Failure(BeemoScanError::custom(
            input,
            "Bad identifier".into(),
        ))),
        Err(e) => Err(e),
    }
}

fn indentation<'a>(input: &'a str, counter: &mut IndentationCounter) -> Result<'a, Vec<Token>> {
    // dbg!("INDENT");
    let (rest, tabs) = many0(tab)(input)?;
    let mut indent_tokens = vec![];
    let indent_level = tabs.len() as isize;
    if indent_level < counter.current {
        for _ in 0..counter.current - indent_level {
            indent_tokens.push(Token::Dedent);
        }
    } else if indent_level > counter.current {
        for _ in 0..indent_level - counter.current {
            indent_tokens.push(Token::Indent);
        }
    }
    indent_tokens.reverse();
    counter.current = indent_level;
    Ok((rest, indent_tokens))
}

fn number(input: &str) -> Result<Token> {
    // Disallow alphabetic chars explicitly to treat the whole thing as a potential bad identifier.
    map(terminated(float, not(alpha1)), |v| Token::Float(v))(input)
}

fn token(input: &str) -> Result<Token> {
    let (input, token) = alt((
        keyword,
        number,
        value(Token::Plus, tag("+")),
        value(Token::Multiply, tag("*")),
        value(Token::Colon, tag(":")),
        value(Token::OpeningParen, tag("(")),
        value(Token::Comma, tag(",")),
        value(Token::ClosingParen, tag(")")),
        value(Token::GreaterThan, tag(">")),
        value(Token::LessThan, tag("<")),
        // Test for identifier only if everything else failed.
        identifier,
    ))(input)?;
    Ok((input, token))
}

fn tokens(input: &str) -> Result<Vec<Token>> {
    not(eof)(input)?;
    let (input, (tokens, _)) = many_till(preceded(space0, token), line_ending)(input)?;
    Ok((input, tokens))
}

fn scan_lines<'a>(
    source: &'a str,
    counter: &mut IndentationCounter,
) -> std::result::Result<Vec<Token>, BeemoError> {
    let indent = |i| indentation(i, counter);
    let full_line = map(pair(indent, tokens), |(mut pre, mut after)| {
        pre.append(&mut after);
        pre
    });

    all_consuming(many_till(full_line, eof))(source)
        .finish()
        .map(|(_, (parsed_lines, _))| parsed_lines.into_iter().flatten().collect())
        .map_err(|e| {
            BeemoError::ScanError(
                e.errors
                    .into_iter()
                    .map(|(i, kind)| (i.to_string(), kind))
                    .collect(),
            )
        })
}

fn scan(source: &str) -> std::result::Result<Vec<Token>, BeemoError> {
    let mut c = IndentationCounter { current: 0 };
    let mut tokens = scan_lines(&source, &mut c)?;
    dbg!(&c);
    for _ in 0..c.current {
        tokens.push(Token::Dedent);
    }
    Ok(tokens)
}

fn main() {
    // As of now, final newline is REQUIRED.
    let source = include_str!("../test.bmo");
    let tokens = scan(&source).unwrap();
    dbg!(&tokens);
    let parser = Parser::new(tokens);
    let res = parser.parse().unwrap();
    dbg!();
    ()
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_success {
        ($source:expr, $exp:expr) => {
            assert_eq!($source, Ok(("", $exp)));
        };
    }

    #[test]
    fn test_float() {
        assert_success!(number("2"), Token::Float(2.0));
        assert_success!(number("3.0"), Token::Float(3.0));
        assert_success!(number("3.0345"), Token::Float(3.0345))
    }

    #[test]
    fn test_identifier() {
        assert_success!(
            identifier("multiply"),
            Token::Identifier(String::from("multiply"))
        );
        assert!(identifier("2multiply").is_err());
        assert!(identifier("-multiply").is_err());
    }

    #[test]
    fn test_keyword() {
        assert_success!(keyword("if"), Token::Keyword("if".into()));
        assert_success!(keyword("return"), Token::Keyword("return".into()));
    }

    #[test]
    fn test_indentation() {
        let mut c = IndentationCounter { current: 0 };
        let source = "a\n\ta\n\t\ta\n\ta\na\n";
        let res = scan_lines(source, &mut c).unwrap();
        assert_eq!(
            res,
            vec![
                Token::Identifier("a".to_string()),
                Token::Indent,
                Token::Identifier("a".to_string()),
                Token::Indent,
                Token::Identifier("a".to_string()),
                Token::Dedent,
                Token::Identifier("a".to_string()),
                Token::Dedent,
                Token::Identifier("a".to_string()),
            ]
        );
    }
}
