use nom;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric0};
use nom::combinator::map;
use nom::multi::{many0, many1};
use nom::sequence::pair;

pub type Result<'a, T> = nom::IResult<&'a str, T, (&'a str, nom::error::ErrorKind)>;

#[derive(Debug)]
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
        map(alt((tag("def"), tag("return"), tag("if"))), |k: &str| {
            Token::Keyword(k.to_string())
        })(input)
    }

    fn identifier(&self, input: &'s str) -> Result<Token> {
        map(pair(alpha1, alphanumeric0), |p| {
            let ident = format!("{}{}", p.0, p.1);
            Token::Identifier(ident)
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

    pub fn new(source: &'s str) -> Self {
        Self { source }
    }
}

fn main() {
    let raw_source = "def return lol derp (boobs poop) hello : vaccine";
    let source: String = raw_source.split_whitespace().collect();
    let scanner = Scanner::new(&source);
    dbg!(scanner.scan());
    ()
}
