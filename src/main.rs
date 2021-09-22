use nom;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::map;

use nom::multi::many0;

pub type Result<'a, T> = nom::IResult<&'a str, T, (&'a str, nom::error::ErrorKind)>;
pub enum Token {
    Identifier(String),
    Keyword(String),
    Colon,
    OpeningParen,
    ClosingParen,
    Eof,
}

struct Scanner<'s> {
    source: &'s str,
}

impl<'s> Scanner<'s> {
    fn keyword(&self, input: &'s str) -> Result<Token> {
        map(alt((tag("def"), tag("return"))), |k: &str| {
            Token::Keyword(k.to_string())
        })(input)
    }

    fn identifier(&self, input: &'s str) -> Result<Token> {
        map(alt((tag("def"), tag("return"))), |i: &str| {
            Token::Identifier(i.to_string())
        })(input)
    }

    fn scan(&self) -> Result<Vec<Token>> {
        let keyword = |i| self.keyword(i);
        let identifier = |i| self.identifier(i);
        let (input, tokens) = many0(alt((
            keyword,
            identifier,
            map(tag(":"), |_| Token::Colon),
            map(tag("("), |_| Token::OpeningParen),
            map(tag(")"), |_| Token::ClosingParen),
        )))(self.source)?;
        Ok((input, tokens))
    }
}

fn main() {
    println!("Hello, world!");
}
