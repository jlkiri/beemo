use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_till;
use nom::bytes::complete::take_while;
use nom::character::complete::alphanumeric1;
use nom::character::complete::anychar;
use nom::character::complete::char;
use nom::character::complete::line_ending;
use nom::character::complete::space0;
use nom::character::complete::space1;
use nom::character::complete::{alpha1, alphanumeric0, tab};
use nom::character::is_alphanumeric;
use nom::combinator::all_consuming;
use nom::combinator::eof;
use nom::combinator::iterator;
use nom::combinator::not;
use nom::combinator::{map, recognize, value};
use nom::error::{ContextError, ErrorKind as NomErrorKind, ParseError};
use nom::multi::many0;
use nom::multi::many_till;
use nom::number::complete::float;
use nom::sequence::delimited;
use nom::sequence::{pair, preceded, terminated};
use nom::Err::Failure;
use nom::Finish;
use nom::Offset;
use nom::{self};

use miette::Diagnostic;
use thiserror::Error;

use crate::error::BeemoError;

pub type Result<'a, T> = nom::IResult<&'a str, T, BeemoScanError<&'a str>>;

#[derive(Debug, PartialEq, Diagnostic, Error, Clone)]
pub enum ErrorKind {
    #[error("{0:?}")]
    Nom(NomErrorKind),
    #[error("{0}")]
    Context(&'static str),
    #[error("{1}")]
    Custom(usize, String),
}

#[derive(Debug, PartialEq)]
pub struct BeemoScanError<I> {
    pub error: (I, ErrorKind),
}

impl<I> ParseError<I> for BeemoScanError<I> {
    fn from_error_kind(input: I, kind: NomErrorKind) -> Self {
        Self {
            error: (input, ErrorKind::Nom(kind)),
        }
    }

    fn append(input: I, kind: NomErrorKind, mut other: Self) -> Self {
        // other.errors.push((input, ErrorKind::Nom(kind)));
        other
    }

    fn from_char(input: I, c: char) -> Self {
        Self {
            error: (input, ErrorKind::Context("char")),
        }
    }
}

impl<I> BeemoScanError<I> {
    pub fn custom(span: usize, input: I, msg: String) -> Self {
        Self {
            error: (input, ErrorKind::Custom(span, msg)),
        }
    }
}

impl<I> ContextError<I> for BeemoScanError<I> {
    fn add_context(input: I, ctx: &'static str, mut other: Self) -> Self {
        // other.errors.push((input, ErrorKind::Context(ctx)));
        other
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub ty: TokenType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Identifier(String),
    Keyword(String),
    String(String),
    Float(f32),
    Colon,
    Plus,
    Multiply,
    Minus,
    Divide,
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

fn keyword(input: &str) -> Result<TokenType> {
    map(alt((tag("return"), tag("print"))), |k: &str| {
        TokenType::Keyword(k.to_string())
    })(input)
}

fn identifier(input: &str) -> Result<TokenType> {
    match map(recognize(pair(alpha1, alphanumeric0)), |s: &str| {
        TokenType::Identifier(s.to_string())
    })(input)
    {
        Ok((rest, t)) => Ok((rest, t)),
        Err(nom::Err::Error(_)) => {
            dbg!(input);
            let (next, _) = many_till::<_, _, _, (), _, _>(anychar, space1)(input)
                .expect("Impossible to find a span.");

            dbg!(next);
            let span = input.offset(next) - 1; // Do not count matched whitespace;
            dbg!(span);
            Err(Failure(BeemoScanError::custom(
                span,
                input,
                "Bad identifier".into(),
            )))
        }
        Err(e) => Err(e),
    }
}

fn indentation<'a>(input: &'a str, counter: &mut IndentationCounter) -> Result<'a, Vec<TokenType>> {
    let (rest, tabs) = many0(tab)(input)?;
    let mut indent_tokens = vec![];
    let indent_level = tabs.len() as isize;
    if indent_level < counter.current {
        for _ in 0..counter.current - indent_level {
            indent_tokens.push(TokenType::Dedent);
        }
    } else if indent_level > counter.current {
        for _ in 0..indent_level - counter.current {
            indent_tokens.push(TokenType::Indent);
        }
    }
    indent_tokens.reverse();
    counter.current = indent_level;
    Ok((rest, indent_tokens))
}

fn number(input: &str) -> Result<TokenType> {
    // Disallow alphabetic chars explicitly to treat the whole thing as a potential bad identifier.
    map(terminated(float, not(alpha1)), |v| TokenType::Float(v))(input)
}

fn string(input: &str) -> Result<TokenType> {
    let (input, token) = char('"')(input)?;
    let (input, value) = take_till(|c| c == '"')(input)?;
    let (input, token) = char::<_, BeemoScanError<&str>>('"')(input).or(Err(Failure(
        BeemoScanError::custom(0, input, r#"Missing closing quote."#.to_string()),
    )))?;
    Ok((input, TokenType::String(value.to_string())))
}

fn maybe_string(input: &str) -> Result<TokenType> {
    alt((string, non_string))(input)
}

fn non_string(input: &str) -> Result<TokenType> {
    let (input, token) = alt((
        value(TokenType::Plus, tag("+")),
        value(TokenType::Multiply, tag("*")),
        value(TokenType::Colon, tag(":")),
        value(TokenType::OpeningParen, tag("(")),
        value(TokenType::Comma, tag(",")),
        value(TokenType::ClosingParen, tag(")")),
        value(TokenType::GreaterThan, tag(">")),
        value(TokenType::LessThan, tag("<")),
        keyword,
        number,
        // Test for identifier only if everything else failed.
        identifier,
    ))(input)?;
    Ok((input, token))
}

fn tokens(input: &str) -> Result<Vec<TokenType>> {
    not(eof)(input)?;
    let (input, (tokens, _)) = many_till(preceded(space0, maybe_string), line_ending)(input)?;
    Ok((input, tokens))
}

fn preceding_lines(input: &str) -> Result<(Vec<()>, &str)> {
    terminated(many_till(value((), anychar), line_ending), not(eof))(input)
}

fn count_preceding_lines(input: &str) -> (&str, usize) {
    let mut iter = iterator(input, preceding_lines);
    let count = iter.count();
    let (rest, _) = iter.finish().finish().expect("Should not fail.");
    (rest, count)
}

fn scan_lines<'a>(
    source: &'a str,
    counter: &mut IndentationCounter,
) -> std::result::Result<Vec<TokenType>, BeemoError> {
    let indent = |i| indentation(i, counter);
    let full_line = map(pair(indent, tokens), |(mut pre, mut after)| {
        pre.append(&mut after);
        pre
    });

    all_consuming(many_till(full_line, eof))(source)
        .finish()
        .map(|(_, (parsed_lines, _))| parsed_lines.into_iter().flatten().collect())
        .map_err(|e| {
            let (unscanned, kind): (&str, ErrorKind) = e.error;
            // Replace tabs with spaces due to miette issues.
            let spaced_source = source.replace('\t', "    ").to_string();
            let global_offset = source.offset(unscanned);
            // let (start, lines) = count_preceding_lines(&source[..=global_offset]);
            // let tab_count = start.matches('\t').count();
            // let tabs = tab_count * 2; // When tab width is 2.
            if let ErrorKind::Custom(span, ..) = kind {
                return BeemoError::ScanError(
                    spaced_source,
                    (global_offset, span),
                    kind.to_owned(),
                );
            }
            BeemoError::ScanError(spaced_source, (global_offset, 0), kind.to_owned())
        })
}

pub fn scan(source: &str) -> std::result::Result<Vec<Token>, BeemoError> {
    let mut c = IndentationCounter { current: 0 };
    let mut tokens = scan_lines(&source, &mut c)?;
    // dbg!(&c);
    for _ in 0..c.current {
        tokens.push(TokenType::Dedent);
    }
    tokens.push(TokenType::Eof);
    Ok(tokens.into_iter().map(|t| Token { ty: t }).collect())
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
        assert_success!(number("2"), TokenType::Float(2.0));
        assert_success!(number("3.0"), TokenType::Float(3.0));
        assert_success!(number("3.0345"), TokenType::Float(3.0345))
    }

    #[test]
    fn test_identifier() {
        assert_success!(
            identifier("multiply"),
            TokenType::Identifier(String::from("multiply"))
        );
        assert!(identifier("2multiply").is_err());
        assert!(identifier("-multiply").is_err());
    }

    #[test]
    fn test_keyword() {
        assert_success!(keyword("if"), TokenType::Keyword("if".into()));
        assert_success!(keyword("return"), TokenType::Keyword("return".into()));
    }

    #[test]
    fn test_indentation() {
        let mut c = IndentationCounter { current: 0 };
        let source = "a\n\ta\n\t\ta\n\ta\na\n";
        let res = scan_lines(source, &mut c).unwrap();
        assert_eq!(
            res,
            vec![
                TokenType::Identifier("a".to_string()),
                TokenType::Indent,
                TokenType::Identifier("a".to_string()),
                TokenType::Indent,
                TokenType::Identifier("a".to_string()),
                TokenType::Dedent,
                TokenType::Identifier("a".to_string()),
                TokenType::Dedent,
                TokenType::Identifier("a".to_string()),
            ]
        );
    }
}
