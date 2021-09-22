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
    current_indentation: isize,
}

fn line<'a, F1: 'a, F2: 'a>(
    before: F1,
    after: F2,
) -> impl FnMut(&'a str) -> Result<(Vec<Token>, Vec<Token>)>
where
    F1: Fn(&'a str) -> Result<Vec<Token>>,
    F2: FnMut(&'a str) -> Result<Vec<Token>>,
{
    pair(before, terminated(after, newline))
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

    fn indentation(&mut self, input: &'s str) -> Result<Vec<Token>> {
        let (rest, tabs) = many0(tab)(input)?;
        let indentation_tokens: Vec<Token> = tabs.into_iter().map(|_| Token::Indent).collect();
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
        let indendation = |i| self.indentation(i);
        let keyword = |i| self.keyword(i);
        let identifier = |i| self.identifier(i);
        let after_indent = |i: &str| {
            many0(alt((
                keyword,
                identifier,
                map(tag(":"), |_| Token::Colon),
                map(tag("("), |_| Token::OpeningParen),
                map(tag(")"), |_| Token::ClosingParen),
            )))(i)
        };
        let full_line = map(
            pair(indendation, terminated(after_indent, newline)),
            |p: (Vec<Token>, Vec<Token>)| {
                p.0.append(&mut p.1);
                p.0
            },
        );
        /*let (input, tokens) = many0(alt((
            keyword,
            identifier,
            map(tag(":"), |_| Token::Colon),
            map(tag("("), |_| Token::OpeningParen),
            map(tag(")"), |_| Token::ClosingParen),
        ))); */
        let (input, b): (&str, Vec<Vec<Token>>) = many0(full_line)(self.source)?;
        Ok((input, b.into_iter().flatten().collect()))
    }

    pub fn new(source: &'s str) -> Self {
        Self {
            source,
            current_indentation: 0,
        }
    }
}

fn main() {
    let raw_source = "def return lol derp (boobs poop) hello : vaccine";
    let source: String = raw_source.split_whitespace().collect();
    let scanner = Scanner::new(&source);
    dbg!(scanner.scan());
    ()
}
