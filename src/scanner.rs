use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_till;
use nom::bytes::complete::take_while;
use nom::character::complete::anychar;
use nom::character::complete::char;
use nom::character::complete::line_ending;
use nom::character::complete::space0;
use nom::character::complete::space1;
use nom::character::complete::{alpha1, tab};
use nom::combinator::all_consuming;
use nom::combinator::eof;
use nom::combinator::not;
use nom::combinator::{map, recognize, value};
use nom::error::{ErrorKind as NomErrorKind, ParseError};
use nom::multi::many0;
use nom::multi::many_till;
use nom::number::complete::float;
use nom::sequence::{pair, preceded, terminated};
use nom::Err::Failure;
use nom::Finish;
use nom::Offset as NomOffset;
use nom::{self};

use thiserror::Error;

use crate::error::BeemoError;

pub type Result<'a, T> = nom::IResult<&'a str, T, NomScanError<&'a str>>;

type Description = String;
type Help = String;
type TokenLength = usize;
type Span = (usize, usize);
type Offset = usize;

#[derive(Debug, PartialEq, Clone)]
pub enum ErrorKind {
    Nom(NomErrorKind),
    Custom(TokenLength, Description, Help),
}

#[derive(Debug, PartialEq, Error)]
pub struct NomScanError<I> {
    pub error: (I, ErrorKind),
}

impl<I> ParseError<I> for NomScanError<I> {
    fn from_error_kind(input: I, kind: NomErrorKind) -> Self {
        Self {
            error: (input, ErrorKind::Nom(kind)),
        }
    }

    fn append(_input: I, _kind: NomErrorKind, other: Self) -> Self {
        // other.errors.push((input, ErrorKind::Nom(kind)));
        other
    }
}

impl<I> NomScanError<I> {
    pub fn custom(span: usize, input: I, desc: String, help: String) -> Self {
        Self {
            error: (input, ErrorKind::Custom(span, desc, help)),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub ty: TokenType,
    pub span: Span,
}

struct ProtoToken<'input>(TokenType, &'input str, Offset);

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Identifier(String),
    Keyword(String),
    String(String),
    Float(f32),
    Push,
    Colon,
    Plus,
    Multiply,
    EqualEqual,
    Minus,
    Modulo,
    Divide,
    Comma,
    Indent,
    Dedent,
    GreaterThan,
    GreaterEqual,
    LessEqual,
    LessThan,
    Assign,
    Bang,
    OpeningParen,
    ClosingParen,
    OpeningBracket,
    ClosingBracket,
    OpeningBrace,
    ClosingBrace,
    Eof,
}

impl TokenType {
    pub fn string_value(&self) -> Option<&str> {
        match self {
            TokenType::Keyword(v) => Some(v),
            TokenType::String(v) => Some(v),
            TokenType::Identifier(v) => Some(v),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct IndentationCounter {
    current: isize,
}

fn keyword(input: &str) -> Result<ProtoToken> {
    let (rest, tt) = map(
        alt((
            tag("return"),
            tag("print"),
            tag("while"),
            tag("if"),
            tag("true"),
            tag("false"),
            tag("else"),
            tag("for"),
            tag("in"),
        )),
        |k: &str| TokenType::Keyword(k.to_string()),
    )(input)?;
    Ok((rest, ProtoToken(tt, input, input.offset(rest))))
}

fn identifier(input: &str) -> Result<ProtoToken> {
    match map(
        recognize(pair(
            alpha1,
            take_while(|c: char| c.is_alphanumeric() || c == '_'),
        )),
        |s: &str| TokenType::Identifier(s.to_string()),
    )(input)
    {
        Ok((rest, t)) => Ok((rest, ProtoToken(t, input, input.offset(rest)))),
        Err(nom::Err::Error(_)) => {
            let (next, _) = many_till::<_, _, _, (), _, _>(anychar, alt((space1, tag("("))))(input)
                .expect("FATAL: Impossible to find a span of the bad identifier.");
            let span = input.offset(next) - 1; // Do not count matched whitespace;
            Err(Failure(NomScanError::custom(
                span,
                input,
                "Incorrect identifier".into(),
                "Identifiers cannot start with numbers. Only alphabetic characters are allowed in this position.".into()
            )))
        }
        Err(e) => Err(e),
    }
}

fn indentation<'a>(
    input: &'a str,
    counter: &mut IndentationCounter,
) -> Result<'a, Vec<ProtoToken>> {
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

fn number(input: &str) -> Result<ProtoToken> {
    // Disallow alphabetic chars explicitly to treat the whole thing as a potential bad identifier.
    let (rest, tt) = map(terminated(float, not(alpha1)), |v| TokenType::Float(v))(input)?;
    Ok((rest, ProtoToken(tt, input, input.offset(rest))))
}

fn string(input: &str) -> Result<ProtoToken> {
    let (lquote, _) = char('"')(input)?;
    let (body, value) = take_till(|c| c == '"')(lquote)?;
    let (rquote, _) =
        char::<_, NomScanError<&str>>('"')(input).or(Err(Failure(NomScanError::custom(
            0,
            lquote,
            r#"Missing closing quote"#.to_string(),
            r#"Did you forget '"'?"#.to_string(),
        ))))?;
    Ok((
        input,
        ProtoToken(
            TokenType::String(value.to_string()),
            input,
            input.offset(rquote),
        ),
    ))
}

fn maybe_string(input: &str) -> Result<ProtoToken> {
    alt((string, non_string))(input)
}

fn complex_symbol(input: &str) -> Result<ProtoToken> {
    let (rest, tt) = alt((
        value(TokenType::Assign, tag("->")),
        value(TokenType::EqualEqual, tag("==")),
        value(TokenType::GreaterEqual, tag(">=")),
        value(TokenType::LessEqual, tag("<=")),
        value(TokenType::Push, tag(">>")),
    ))(input)?;
    Ok((rest, ProtoToken(tt, input, input.offset(rest))))
}

fn op_bracket(input: &str) -> Result<ProtoToken> {
    let (rest, tt) = value(TokenType::OpeningBracket, tag("["))(input)?;
    Ok((rest, ProtoToken(tt, input, input.offset(rest))))
}

fn cl_bracket(input: &str) -> Result<ProtoToken> {
    let (rest, tt) = value(TokenType::ClosingBracket, tag("]"))(input)?;
    Ok((rest, ProtoToken(tt, input, input.offset(rest))))
}

fn op_brace(input: &str) -> Result<ProtoToken> {
    let (rest, tt) = value(TokenType::OpeningBrace, tag("{"))(input)?;
    Ok((rest, ProtoToken(tt, input, input.offset(rest))))
}

fn cl_brace(input: &str) -> Result<ProtoToken> {
    let (rest, tt) = value(TokenType::ClosingBrace, tag("}"))(input)?;
    Ok((rest, ProtoToken(tt, input, input.offset(rest))))
}

fn op_paren(input: &str) -> Result<ProtoToken> {
    let (rest, tt) = value(TokenType::OpeningParen, tag("("))(input)?;
    Ok((rest, ProtoToken(tt, input, input.offset(rest))))
}

fn cl_paren(input: &str) -> Result<ProtoToken> {
    let (rest, tt) = value(TokenType::ClosingParen, tag(")"))(input)?;
    Ok((rest, ProtoToken(tt, input, input.offset(rest))))
}

fn plus(input: &str) -> Result<ProtoToken> {
    let (rest, tt) = value(TokenType::Plus, tag("+"))(input)?;
    Ok((rest, ProtoToken(tt, input, input.offset(rest))))
}

fn multiply(input: &str) -> Result<ProtoToken> {
    let (rest, tt) = value(TokenType::Multiply, tag("*"))(input)?;
    Ok((rest, ProtoToken(tt, input, input.offset(rest))))
}

fn minus(input: &str) -> Result<ProtoToken> {
    let (rest, tt) = value(TokenType::Minus, tag("-"))(input)?;
    Ok((rest, ProtoToken(tt, input, input.offset(rest))))
}

fn divide(input: &str) -> Result<ProtoToken> {
    let (rest, tt) = value(TokenType::Divide, tag("/"))(input)?;
    Ok((rest, ProtoToken(tt, input, input.offset(rest))))
}

fn colon(input: &str) -> Result<ProtoToken> {
    let (rest, tt) = value(TokenType::Colon, tag(":"))(input)?;
    Ok((rest, ProtoToken(tt, input, input.offset(rest))))
}

fn bang(input: &str) -> Result<ProtoToken> {
    let (rest, tt) = value(TokenType::Bang, tag("!"))(input)?;
    Ok((rest, ProtoToken(tt, input, input.offset(rest))))
}

fn comma(input: &str) -> Result<ProtoToken> {
    let (rest, tt) = value(TokenType::Comma, tag(","))(input)?;
    Ok((rest, ProtoToken(tt, input, input.offset(rest))))
}

fn modulo(input: &str) -> Result<ProtoToken> {
    let (rest, tt) = value(TokenType::Modulo, tag("%"))(input)?;
    Ok((rest, ProtoToken(tt, input, input.offset(rest))))
}

fn gt(input: &str) -> Result<ProtoToken> {
    let (rest, tt) = value(TokenType::GreaterThan, tag(">"))(input)?;
    Ok((rest, ProtoToken(tt, input, input.offset(rest))))
}

fn lt(input: &str) -> Result<ProtoToken> {
    let (rest, tt) = value(TokenType::LessThan, tag("<"))(input)?;
    Ok((rest, ProtoToken(tt, input, input.offset(rest))))
}

fn non_string(input: &str) -> Result<ProtoToken> {
    let (rest, token) = alt((
        complex_symbol,
        op_bracket,
        cl_bracket,
        op_brace,
        cl_brace,
        op_paren,
        cl_paren,
        plus,
        multiply,
        minus,
        divide,
        colon,
        bang,
        comma,
        modulo,
        gt,
        lt,
        keyword,
        number,
        // Test for identifier only if everything else failed.
        identifier,
    ))(input)?;
    Ok((rest, token))
}

fn tokens(input: &str) -> Result<Vec<ProtoToken>> {
    not(eof)(input)?;
    let (input, (tokens, _)) = many_till(preceded(space0, maybe_string), line_ending)(input)?;
    Ok((input, tokens))
}

fn scan_lines(
    source: &str,
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
            let before = &source[..source.offset(unscanned)];
            let tab_count = before.matches('\t').count();
            let spaced_source = source.replace('\t', " ".repeat(4).as_str()).to_string();
            let global_offset = source.offset(unscanned) + tab_count * 4 - tab_count;

            if let ErrorKind::Custom(token_len, desc, help) = kind {
                dbg!((global_offset, token_len));
                return BeemoError::ScanError(
                    spaced_source,
                    (global_offset, token_len),
                    desc,
                    help,
                );
            }
            BeemoError::ScanError(
                spaced_source,
                (global_offset, 0),
                "Unexpected token".into(),
                "This is likely an internal scanner error.".into(),
            )
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
