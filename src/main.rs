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
use nom::{self};

pub type Result<'a, T> = nom::IResult<&'a str, T, BeemoError<&'a str>>;
pub type Error = Box<dyn std::error::Error>;

#[derive(Debug)]
pub enum ErrorKind {
    Nom(NomErrorKind),
    Context(&'static str),
    Custom(String),
}

#[derive(Debug)]
pub struct BeemoError<I> {
    pub errors: Vec<(I, ErrorKind)>,
}

impl<I> ParseError<I> for BeemoError<I> {
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

impl<I> BeemoError<I> {
    pub fn custom(input: I, msg: String) -> Self {
        Self {
            errors: vec![(input, ErrorKind::Custom(msg))],
        }
    }
}

impl<I> ContextError<I> for BeemoError<I> {
    fn add_context(input: I, ctx: &'static str, mut other: Self) -> Self {
        other.errors.push((input, ErrorKind::Context(ctx)));
        other
    }
}

#[derive(Debug, Clone)]
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
    OpeningParen,
    ClosingParen,
    Eof,
}

#[derive(Debug)]
struct IndentationCounter {
    current: isize,
}

fn keyword(input: &str) -> Result<Token> {
    map(alt((tag("def"), tag("return"), tag("if"))), |k: &str| {
        Token::Keyword(k.to_string())
    })(input)
}

fn identifier(input: &str) -> Result<Token> {
    match map(recognize(pair(alpha1, alphanumeric0)), |s: &str| {
        Token::Identifier(s.to_string())
    })(input)
    {
        Ok((rest, t)) => Ok((rest, t)),
        Err(nom::Err::Error(_)) => Err(Failure(BeemoError::custom(input, "Bad identifier".into()))),
        Err(e) => Err(e),
    }
}

fn indentation<'a>(input: &'a str, counter: &mut IndentationCounter) -> Result<'a, Vec<Token>> {
    // dbg!("INDENT");
    let (rest, tabs) = many0(tab)(input)?;
    let mut indent_tokens = tabs.into_iter().map(|_| Token::Indent).collect::<Vec<_>>();
    let indent_level = indent_tokens.len() as isize;
    if indent_level < counter.current {
        for _ in 0..counter.current - indent_level {
            indent_tokens.push(Token::Dedent);
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

#[derive(Debug)]
enum ScanResult {
    Success(Vec<Token>),
    Failure,
}

fn scan_lines(
    source: &str,
    counter: &mut IndentationCounter,
) -> std::result::Result<ScanResult, Error> {
    let indent = |i| indentation(i, counter);
    let full_line = map(
        // pair(indent, terminated(after_indent, line_ending)),
        pair(indent, tokens),
        |(mut pre, mut after)| {
            pre.append(&mut after);
            pre
        },
    );
    let (_, (parsed_lines, _)) = many_till(full_line, eof)(source).expect("handle errors properly");
    Ok(ScanResult::Success(
        parsed_lines.into_iter().flatten().collect(),
    ))
}

fn scan(source: &str) -> std::result::Result<Vec<Token>, Error> {
    let mut c = IndentationCounter { current: 0 };
    let scan_result = scan_lines(&source, &mut c)?;
    dbg!(&c);
    match scan_result {
        ScanResult::Success(mut tokens) => {
            for _ in 0..c.current {
                tokens.push(Token::Dedent);
            }
            Ok(tokens)
        }
        ScanResult::Failure => Err("fail".into()),
    }
}

fn space_alph(s: &str) -> Result<&str> {
    match alpha1(s) {
        Ok(r) => Ok(r),
        Err(nom::Err::Error(_)) => Err(Failure(BeemoError::custom(s, "Bad identifier".into()))),
        Err(e) => Err(e),
    }
}

fn parse_test(s: &str) -> Result<Vec<&str>> {
    let (input, res) = many0(preceded(space0, space_alph))(s)?;
    let (input, _) = context("bad identifier", line_ending)(input)?;
    Ok((input, res))
}

fn main() {
    // As of now, final newline is REQUIRED.
    let source = include_str!("../test.bmo");
    dbg!(scan(&source));
    ()
}
