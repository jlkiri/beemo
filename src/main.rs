use nom;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric0, multispace0, newline, tab};
use nom::combinator::{map, opt, recognize};
use nom::multi::{many0, many1};
use nom::sequence::{delimited, pair, preceded, terminated};

pub type Result<'a, T> = nom::IResult<&'a str, T>;

#[derive(Debug)]
pub enum Token {
    Identifier(String),
    Keyword(String),
    Colon,
    Indent,
    Dedent,
    OpeningParen,
    ClosingParen,
    Eof,
}

struct Scanner<'s> {
    source: &'s str,
    current_indentation: isize
}

fn line<'a, F: 'a>(inner: F) -> impl FnMut(&'a str) -> Result<(Vec<char>, &'a str)>
where
    F: Fn(&'a str) -> Result<&'a str>,
{
    pair(many0(tab), terminated(inner, newline))
}

impl<'s> Scanner<'s> {
    fn keyword(&self, input: &'s str) -> Result<Token> {
        map(alt((tag("def"), tag("return"), tag("if"))), |k: &str| {
            Token::Keyword(k.to_string())
        })(input)
    }

    fn identifier(&self, input: &'s str) -> Result<Token> {
        map(recognize(pair(alpha1, alphanumeric0)), |s: &str| {
            Token::Identifier(s.to_string())
        })(input)
    }

    fn indendation(&mut self, input: &'s str) -> Result<Vec<Token>> {
        let (rest, tabs) = many0(tab)(input)?;
        let indentation_tokens: Vec<Token> = tabs.into_iter().map(|t| Token::Indent).collect();
        let parsed_indent = indentation_tokens.len() as isize;
        if parsed_indent < self.current_indentation {
            for _ in 0..self.current_indentation - parsed_indent {
                indentation_tokens.push(Token::Dedent);
            }
        }
        indentation_tokens.reverse();
        self.current_indentation = parsed_indent;
        Ok((rest, indentation_tokens))
    }

    fn scan(&self) -> Result<Vec<Token>> {
        let line = pair(, terminated(inner, newline));
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
        Self { source, current_indentation: 0 }
    }
}

fn main() {
    let raw_source = "def return lol derp (boobs poop) hello : vaccine";
    let source: String = raw_source.split_whitespace().collect();
    let scanner = Scanner::new(&source);
    dbg!(scanner.scan());
    ()
}
