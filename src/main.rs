use nom;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric0, multispace0, newline as lf, tab};
use nom::character::complete::{crlf, space0};
use nom::combinator::{map, opt, recognize, value};
use nom::multi::{many0, many1};
use nom::sequence::{delimited, pair, preceded, terminated};

pub type Result<'a, T> = nom::IResult<&'a str, T>;

#[derive(Debug)]
pub enum Token {
    Identifier(String),
    Keyword(String),
    Colon,
    Plus,
    Multiply,
    Indent,
    Dedent,
    OpeningParen,
    ClosingParen,
    Eof,
}

struct IndentationCounter {
    current_indentation: isize,
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
    dbg!(rest);
    let mut indentation_tokens: Vec<Token> = tabs.into_iter().map(|_| Token::Indent).collect();
    let parsed_indent = indentation_tokens.len() as isize;
    if parsed_indent < counter.current_indentation {
        for _ in 0..counter.current_indentation - parsed_indent {
            indentation_tokens.push(Token::Dedent);
        }
    }
    indentation_tokens.reverse();
    counter.current_indentation = parsed_indent;
    Ok((rest, indentation_tokens))
}

fn after_indent(input: &str) -> nom::IResult<&str, Vec<Token>> {
    many0(preceded(
        space0,
        alt((
            keyword,
            identifier,
            map(tag("+"), |_| Token::Plus),
            map(tag("*"), |_| Token::Multiply),
            map(tag(":"), |_| Token::Colon),
            map(tag("("), |_| Token::OpeningParen),
            map(tag(")"), |_| Token::ClosingParen),
        )),
    ))(input)
}

fn newline(input: &str) -> nom::IResult<&str, Vec<&str>> {
    many1(alt((value("", lf), crlf)))(input)
}

fn scan(source: &str) -> Result<Vec<Token>> {
    let mut c = IndentationCounter {
        current_indentation: 0,
    };
    let indent = |i| indentation(i, &mut c);
    // let after_indent = ;
    let full_line = map(pair(indent, terminated(after_indent, newline)), |mut p| {
        dbg!(&p.1);
        p.0.append(&mut p.1);
        p.0
    });
    let (input, lines) = many0(full_line)(source)?;
    Ok((input, lines.into_iter().flatten().collect()))
}

fn main() {
    let source = include_str!("../test.bmo");
    let s = "def func(a, b):\n\treturn a + b\r\n\r\ndef func2(c, d):\r\n\treturn c * d\r\n";
    dbg!(&source);
    dbg!(scan(&source));
    ()
}
