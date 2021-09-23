use std::convert::Infallible;

use nom;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric0, multispace0, newline as lf, tab};
use nom::character::complete::{crlf, space0};
use nom::combinator::{map, map_res, opt, recognize, value};
use nom::multi::{many0, many1};
use nom::number::complete::f64;
use nom::number::complete::float;
use nom::sequence::{delimited, pair, preceded, terminated};

pub type Result<'a, T> = nom::IResult<&'a str, T>;
pub type Error = Box<dyn std::error::Error>;

#[derive(Debug)]
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
    remainder: isize,
}

fn keyword(input: &str) -> Result<Token> {
    map(alt((tag("def"), tag("return"), tag("if"))), |k: &str| {
        Token::Keyword(k.to_string())
    })(input)
}

fn identifier(input: &str) -> Result<Token> {
    map(recognize(pair(alpha1, alphanumeric0)), |s: &str| {
        Token::Identifier(s.to_string())
    })(input)
}

fn indentation<'a>(
    input: &'a str,
    counter: &mut IndentationCounter,
) -> nom::IResult<&'a str, Vec<Token>> {
    let (rest, tabs) = many0(tab)(input)?;
    let mut indent_tokens = tabs.into_iter().map(|_| Token::Indent).collect::<Vec<_>>();
    let indent_level = indent_tokens.len() as isize;
    if indent_level < counter.current {
        for _ in 0..counter.current - indent_level {
            indent_tokens.push(Token::Dedent);
        }
    }
    indent_tokens.reverse();
    counter.remainder = counter.current;
    counter.current = indent_level;
    Ok((rest, indent_tokens))
}

fn after_indent(input: &str) -> nom::IResult<&str, Vec<Token>> {
    many1(preceded(
        space0,
        alt((
            keyword,
            identifier,
            map(float, |v| Token::Float(v)),
            map(tag("+"), |_| Token::Plus),
            map(tag("*"), |_| Token::Multiply),
            map(tag(":"), |_| Token::Colon),
            map(tag("("), |_| Token::OpeningParen),
            map(tag(","), |_| Token::Comma),
            map(tag(")"), |_| Token::ClosingParen),
        )),
    ))(input)
}

fn newline(input: &str) -> nom::IResult<&str, Vec<&str>> {
    many1(alt((value("", lf), crlf)))(input)
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
    let full_line = map(pair(indent, terminated(after_indent, newline)), |mut p| {
        p.0.append(&mut p.1);
        p.0
    });
    let (_, parsed_lines) = many0(full_line)(source).expect("handle errors properly");
    Ok(ScanResult::Success(
        parsed_lines.into_iter().flatten().collect(),
    ))
}

fn scan(source: &str) -> std::result::Result<Vec<Token>, Error> {
    let mut c = IndentationCounter {
        remainder: 0,
        current: 0,
    };
    let scan_result = scan_lines(&source, &mut c)?;
    match scan_result {
        ScanResult::Success(mut tokens) => {
            for _ in 0..c.remainder {
                tokens.push(Token::Dedent);
            }
            Ok(tokens)
        }
        ScanResult::Failure => Err("fail".into()),
    }
}

fn main() {
    // As of now, final newline is REQUIRED.
    let source = include_str!("../test.bmo");
    dbg!(scan(&source));
    ()
}
