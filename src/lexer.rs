use std::iter::Peekable;
use std::fmt;
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;
use crate::errors::LexError;

#[derive(PartialEq, Debug, Clone)]
pub enum TokenType {
    // operators 
    Plus, Minus, Star, Slash, EqualEqual, Equal, PlusPlus, Greater, Less,
     GreaterEqual, LessEqual, Bang, BangEqual,

    // Grouping
    LeftParen, RightParen,

    // Keywords
    Identifier,

    // Prims
    NUMBER, STRING, TRUE, FALSE, Nil,

    EOF
}

#[derive(PartialEq, Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: u64,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        write!(f, "'{}'", self.lexeme)
    }
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: u64) -> Token {
        Token { token_type, lexeme, line }
    }
}

#[allow(dead_code)]
pub fn lex_file(filename: &str) -> Result<Vec<Token>, LexError> {
    let file = File::open(filename).unwrap();
    let buf_reader = BufReader::new(file);
    let mut tokens = Vec::new();
    let mut max = 0;
    for (i, val) in buf_reader.lines().enumerate() {
        match val {
            Ok(line) => tokens.append(&mut lex(line, i as u64)?),
            Err(e) => return Err(LexError::new(i as u64, format!("Error reading from file: {}", e))),
        }
        max = i;
    }
    // Add EOF: 1 is a dummy value that is never used
    tokens.push(Token::new(TokenType::EOF, String::new(), max as u64));
    Ok(tokens)
}


pub fn lex_line(line: String) -> Result<Vec<Token>, LexError> {
    let mut tokens = lex(line, 1)?;
    tokens.push(Token::new(TokenType::EOF, String::new(), 1));
    Ok(tokens)
}

fn lex(line: String, line_num: u64) -> Result<Vec<Token>, LexError> {
    let mut tokens = Vec::new();
    let mut it = line.chars().peekable();

    while let Some(c) = it.peek() {
        match c {
            '0'..='9' => add_token(get_number(line_num, &mut it), &mut tokens),
            '"' => add_token(get_string(line_num, &mut it), &mut tokens),
            '(' => add_and_consume(Token::new(TokenType::LeftParen, c.to_string(), line_num), &mut tokens, &mut it),
            ')' => add_and_consume(Token::new(TokenType::RightParen, c.to_string(), line_num), &mut tokens, &mut it),
            '-' => add_and_consume(Token::new(TokenType::Minus, c.to_string(), line_num), &mut tokens, &mut it),
            '*' => add_and_consume(Token::new(TokenType::Star, c.to_string(), line_num), &mut tokens, &mut it),
            '/' => add_and_consume(Token::new(TokenType::Slash, c.to_string(), line_num), &mut tokens, &mut it),
            '+' => check_ahead_and_add(&mut tokens, line_num, &mut it)?,
            '=' => check_ahead_and_add(&mut tokens, line_num, &mut it)?,
            '!' => check_ahead_and_add(&mut tokens, line_num, &mut it)?,
            '>' => check_ahead_and_add(&mut tokens, line_num, &mut it)?,
            '<' => check_ahead_and_add(&mut tokens, line_num, &mut it)?,
            'A'..='Z' | 'a'..='z' => add_identifier(&mut tokens, line_num, &mut it)?,
            ' ' => {it.next();},
            _ => return Err(LexError::new(line_num, format!("Invalid Character '{}'", c))),
        }
    }
    Ok(tokens)
}


fn add_and_consume<I: Iterator<Item=char>>(token: Token, v: &mut Vec<Token>, it: &mut Peekable<I>) {
    v.push(token);
    it.next();
}

fn check_ahead_and_add<I: Iterator<Item=char>>(tokens: &mut Vec<Token>, line_num: u64, it: &mut Peekable<I>) -> Result<(), LexError> {
    let token: String = take_op(it);
    let t = match &token[..] {
        ">" => Token::new(TokenType::Greater, token, line_num),
        "<" => Token::new(TokenType::Less, token, line_num),
        "+" => Token::new(TokenType::Plus, token, line_num),
        "!" => Token::new(TokenType::Bang, token, line_num),
        "=" => Token::new(TokenType::Equal, token, line_num),
        ">=" => Token::new(TokenType::GreaterEqual, token, line_num),
        "<=" => Token::new(TokenType::LessEqual, token, line_num),
        "==" => Token::new(TokenType::EqualEqual, token, line_num),
        "++" => Token::new(TokenType::PlusPlus, token, line_num),
        "!=" => Token::new(TokenType::BangEqual, token, line_num),
        _ => return Err(LexError::new(line_num, format!("Invalid Character '{}'", token))),
    };
    add_token(t, tokens);
    Ok(())
}


fn get_string<I: Iterator<Item=char>>(line_num: u64, it: &mut Peekable<I>) -> Token {
    it.next(); // Consume the leading "
    let string = it.take_while(|c| *c != '"').collect::<String>();
    Token::new(TokenType::STRING, string, line_num)
}

fn take_op<I: Iterator<Item=char>>(it: &mut Peekable<I>) -> String {
    let mut s = String::new();
    while let Some(val) = it.peek() {
        match val {
            '>' | '=' | '<' | '!'| '+' => s.push(*val),
            _=> break,
        }
        it.next();
    }
    return s;
}

fn add_identifier<I: Iterator<Item=char>>(tokens: &mut Vec<Token>, line_num: u64, it: &mut Peekable<I>) -> Result<(), LexError> {
    let identifier = it.take_while(|c| ('A'..='Z').contains(c) || ('a'..='z').contains(c)).collect::<String>();
    let identifier_type = determine_identifier(&identifier);
    add_token(Token::new(identifier_type, identifier, line_num), tokens);
    Ok(())
}

fn determine_identifier(s: &String) -> TokenType {
    match &s[..] {
        "true" => TokenType::TRUE,
        "false" => TokenType::FALSE,
        "nil" => TokenType::Nil,
        _ => TokenType::Identifier,
    }
}


fn add_token(token: Token, v: &mut Vec<Token>) {
    v.push(token);
}

fn get_number<I: Iterator<Item=char>>(line_num: u64, it: &mut Peekable<I>) -> Token {
    let mut num = String::new();
    while let Some(val) = it.peek() {
        match val {
            '0'..='9' | '.' => {
                num.push(*val);
                it.next();
            }
            _ => break,
        }
    }
    Token::new(TokenType::NUMBER, num, line_num)
}



#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn lex_addition() {
        let tokens = lex_line("1 + 9 - 3".to_string()).unwrap();
        let expected = vec![
            Token::new(TokenType::NUMBER, "1".to_string(), 1),
            Token::new(TokenType::Plus, "+".to_string(), 1),
            Token::new(TokenType::NUMBER, "9".to_string(), 1),
            Token::new(TokenType::Minus, "-".to_string(), 1),
            Token::new(TokenType::NUMBER, "3".to_string(), 1),
            Token::new(TokenType::EOF, String::new(), 1),
        ];

        assert_eq!(expected, tokens);
    }

    #[test]
    fn lex_mult_and_div() {
        let tokens = lex_line("3 * 9 / 4".to_string()).unwrap();
        let expected = vec![
            Token::new(TokenType::NUMBER, "3".to_string(), 1),
            Token::new(TokenType::Star, "*".to_string(), 1),
            Token::new(TokenType::NUMBER, "9".to_string(), 1),
            Token::new(TokenType::Slash, "/".to_string(), 1),
            Token::new(TokenType::NUMBER, "4".to_string(), 1),
            Token::new(TokenType::EOF, String::new(), 1),
        ];

        assert_eq!(expected, tokens);
    }

    #[test]
    fn lex_numbers() {
        let tokens = lex_line("12 3 67.89".to_string()).unwrap();
        let expected = vec![
            Token::new(TokenType::NUMBER, "12".to_string(), 1),
            Token::new(TokenType::NUMBER, "3".to_string(), 1),
            Token::new(TokenType::NUMBER, "67.89".to_string(), 1),
            Token::new(TokenType::EOF, String::new(), 1),
        ];

        assert_eq!(expected, tokens);
    }

    #[test]
    fn lex_with_brackets() {
        let tokens = lex_line("((1 + 2) / 3) * 4".to_string()).unwrap();
        let expected = vec![
            Token::new(TokenType::LeftParen, "(".to_string(), 1),
            Token::new(TokenType::LeftParen, "(".to_string(), 1),
            Token::new(TokenType::NUMBER, "1".to_string(), 1),
            Token::new(TokenType::Plus, "+".to_string(), 1),
            Token::new(TokenType::NUMBER, "2".to_string(), 1),
            Token::new(TokenType::RightParen, ")".to_string(), 1),
            Token::new(TokenType::Slash, "/".to_string(), 1),
            Token::new(TokenType::NUMBER, "3".to_string(), 1),
            Token::new(TokenType::RightParen, ")".to_string(), 1),
            Token::new(TokenType::Star, "*".to_string(), 1),
            Token::new(TokenType::NUMBER, "4".to_string(), 1),
            Token::new(TokenType::EOF, String::new(), 1),
        ];

        assert_eq!(expected, tokens);
    }

    #[test]
    fn lex_with_look_ahead() {
        let tokens = lex_line(">= > + ++ < <= ! != = ==".to_string()).unwrap();
        let expected = vec![
            Token::new(TokenType::GreaterEqual, ">=".to_string(), 1),
            Token::new(TokenType::Greater, ">".to_string(), 1),
            Token::new(TokenType::Plus, "+".to_string(), 1),
            Token::new(TokenType::PlusPlus, "++".to_string(), 1),
            Token::new(TokenType::Less, "<".to_string(), 1),
            Token::new(TokenType::LessEqual, "<=".to_string(), 1),
            Token::new(TokenType::Bang, "!".to_string(), 1),
            Token::new(TokenType::BangEqual, "!=".to_string(), 1),
            Token::new(TokenType::Equal, "=".to_string(), 1),
            Token::new(TokenType::EqualEqual, "==".to_string(), 1),
            Token::new(TokenType::EOF, String::new(), 1),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn lex_with_strings() {
        let tokens = lex_line("\"Hello\" \"hELLo\" \"hello\" \"HELLO\" \"\"".to_string()).unwrap();
        let expected = vec![
            Token::new(TokenType::STRING, "Hello".to_string(), 1),
            Token::new(TokenType::STRING, "hELLo".to_string(), 1),
            Token::new(TokenType::STRING, "hello".to_string(), 1),
            Token::new(TokenType::STRING, "HELLO".to_string(), 1),
            Token::new(TokenType::STRING, "".to_string(), 1),
            Token::new(TokenType::EOF, String::new(), 1),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn lex_no_space() {
        let tokens = lex_line("5+6".to_string()).unwrap();
        let expected = vec![
            Token::new(TokenType::NUMBER, "5".to_string(), 1),
            Token::new(TokenType::Plus, "+".to_string(), 1),
            Token::new(TokenType::NUMBER, "6".to_string(), 1),
            Token::new(TokenType::EOF, String::new(), 1),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn boolean_identifiers() {
        let tokens = lex_line("true false nil".to_string()).unwrap();
        let expected = vec![
            Token::new(TokenType::TRUE, "true".to_string(), 1),
            Token::new(TokenType::FALSE, "false".to_string(), 1),
            Token::new(TokenType::Nil, "nil".to_string(), 1),
            Token::new(TokenType::EOF, String::new(), 1),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn variable_identifiers() {
        let tokens = lex_line("josh x y z".to_string()).unwrap();
        let expected = vec![
            Token::new(TokenType::Identifier, "josh".to_string(), 1),
            Token::new(TokenType::Identifier, "x".to_string(), 1),
            Token::new(TokenType::Identifier, "y".to_string(), 1),
            Token::new(TokenType::Identifier, "z".to_string(), 1),
            Token::new(TokenType::EOF, String::new(), 1),
        ];
        assert_eq!(expected, tokens);
    }
}