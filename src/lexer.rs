use std::iter::Peekable;
use std::fmt;
#[derive(PartialEq, Debug, Clone)]
pub enum TokenType {
    // operators 
    Plus, Minus, Star, Slash, EqualEqual, Equal, PlusPlus, Greater, Less,
     GreaterEqual, LessEqual, Bang, BangEqual,

    // Grouping
    LeftParen, RightParen,

    // Prims
    NUMBER, STRING, TRUE, FALSE, Nil,

    EOF
}

#[derive(PartialEq, Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        write!(f, "{}", self.lexeme)
    }
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String) -> Token {
        Token { token_type, lexeme }
    }
}


pub fn lex(line: String) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut it = line.chars().peekable();

    while let Some(c) = it.peek() {
        match c {
            '0'..='9' => add_token(get_number(&mut it), &mut tokens),
            '"' => add_token(get_string(&mut it), &mut tokens),
            '(' => add_and_cont(Token::new(TokenType::LeftParen, c.to_string()), &mut tokens, &mut it),
            ')' => add_and_cont(Token::new(TokenType::RightParen, c.to_string()), &mut tokens, &mut it),
            '-' => add_and_cont(Token::new(TokenType::Minus, c.to_string()), &mut tokens, &mut it),
            '*' => add_and_cont(Token::new(TokenType::Star, c.to_string()), &mut tokens, &mut it),
            '/' => add_and_cont(Token::new(TokenType::Slash, c.to_string()), &mut tokens, &mut it),
            '+' => check_ahead(&mut tokens, &mut it),
            '=' => check_ahead(&mut tokens, &mut it),
            '!' => check_ahead(&mut tokens, &mut it),
            '>' => check_ahead(&mut tokens, &mut it),
            '<' => check_ahead(&mut tokens, &mut it),
            'A'..='Z' | 'a'..='z' => add_identifier(&mut tokens, &mut it),
            ' ' => {it.next();},
            _ => panic!("Invalid token {}", c),
        }
    }
    tokens.push(Token::new(TokenType::EOF, String::new()));
    tokens
}


fn add_and_cont<I: Iterator<Item=char>>(token: Token, v: &mut Vec<Token>, it: &mut Peekable<I>) {
    v.push(token);
    it.next();
}

fn check_ahead<I: Iterator<Item=char>>(tokens: &mut Vec<Token>, it: &mut Peekable<I>) {
    let token: String = take_op(it);
    match &token[..] {
        ">" => add_token(Token::new(TokenType::Greater, token), tokens),
        "<" => add_token(Token::new(TokenType::Less, token), tokens),
        "+" => add_token(Token::new(TokenType::Plus, token), tokens),
        "!" => add_token(Token::new(TokenType::Bang, token), tokens),
        "=" => add_token(Token::new(TokenType::Equal, token), tokens),
        ">=" => add_token(Token::new(TokenType::GreaterEqual, token), tokens),
        "<=" => add_token(Token::new(TokenType::LessEqual, token), tokens),
        "==" => add_token(Token::new(TokenType::EqualEqual, token), tokens),
        "++" => add_token(Token::new(TokenType::PlusPlus, token), tokens),
        "!=" => add_token(Token::new(TokenType::BangEqual, token), tokens),
        _ => panic!("Invalid token"),
    }
}


fn get_string<I: Iterator<Item=char>>(it: &mut Peekable<I>) -> Token {
    it.next(); // Consume the "
    let string = it.take_while(|c| *c != '"').collect::<String>();
    Token::new(TokenType::STRING, string)
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

fn add_identifier<I: Iterator<Item=char>>(tokens: &mut Vec<Token>, it: &mut Peekable<I>) {
    let identifier = it.take_while(|c| ('A'..='Z').contains(c) || ('a'..='z').contains(c)).collect::<String>();
    let identifier_type = determine_identifier(&identifier);
    add_token(Token::new(identifier_type, identifier), tokens);
}

fn determine_identifier(s: &String) -> TokenType {
    match &s[..] {
        "true" => TokenType::TRUE,
        "false" => TokenType::FALSE,
        "nil" => TokenType::Nil,
        _ => panic!("Invalid Identifier. Given {}", s),
    }
}


fn add_token(token: Token, v: &mut Vec<Token>) {
    v.push(token);
}

fn get_number<I: Iterator<Item=char>>(it: &mut Peekable<I>) -> Token {
    let mut num = String::new();
    while let Some(val) = it.peek() {
        match val {
            '0'..='9' => {
                num.push(*val);
                it.next();
            }
            _ => break,
        }
    }
    Token::new(TokenType::NUMBER, num)
}



#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn lex_addition() {
        let tokens = lex("1 + 9 - 3".to_string());
        let expected = vec![
            Token::new(TokenType::NUMBER, "1".to_string()),
            Token::new(TokenType::Plus, "+".to_string()),
            Token::new(TokenType::NUMBER, "9".to_string()),
            Token::new(TokenType::Minus, "-".to_string()),
            Token::new(TokenType::NUMBER, "3".to_string()),
            Token::new(TokenType::EOF, String::new()),
        ];

        assert_eq!(expected, tokens);
    }

    #[test]
    fn lex_mult_and_div() {
        let tokens = lex("3 * 9 / 4".to_string());
        let expected = vec![
            Token::new(TokenType::NUMBER, "3".to_string()),
            Token::new(TokenType::Star, "*".to_string()),
            Token::new(TokenType::NUMBER, "9".to_string()),
            Token::new(TokenType::Slash, "/".to_string()),
            Token::new(TokenType::NUMBER, "4".to_string()),
            Token::new(TokenType::EOF, String::new()),
        ];

        assert_eq!(expected, tokens);
    }

    #[test]
    fn lex_with_brackets() {
        let tokens = lex("((1 + 2) / 3) * 4".to_string());
        let expected = vec![
            Token::new(TokenType::LeftParen, "(".to_string()),
            Token::new(TokenType::LeftParen, "(".to_string()),
            Token::new(TokenType::NUMBER, "1".to_string()),
            Token::new(TokenType::Plus, "+".to_string()),
            Token::new(TokenType::NUMBER, "2".to_string()),
            Token::new(TokenType::RightParen, ")".to_string()),
            Token::new(TokenType::Slash, "/".to_string()),
            Token::new(TokenType::NUMBER, "3".to_string()),
            Token::new(TokenType::RightParen, ")".to_string()),
            Token::new(TokenType::Star, "*".to_string()),
            Token::new(TokenType::NUMBER, "4".to_string()),
            Token::new(TokenType::EOF, String::new()),
        ];

        assert_eq!(expected, tokens);
    }

    #[test]
    fn lex_with_look_ahead() {
        let tokens = lex(">= > + ++ < <= ! != = ==".to_string());
        let expected = vec![
            Token::new(TokenType::GreaterEqual, ">=".to_string()),
            Token::new(TokenType::Greater, ">".to_string()),
            Token::new(TokenType::Plus, "+".to_string()),
            Token::new(TokenType::PlusPlus, "++".to_string()),
            Token::new(TokenType::Less, "<".to_string()),
            Token::new(TokenType::LessEqual, "<=".to_string()),
            Token::new(TokenType::Bang, "!".to_string()),
            Token::new(TokenType::BangEqual, "!=".to_string()),
            Token::new(TokenType::Equal, "=".to_string()),
            Token::new(TokenType::EqualEqual, "==".to_string()),
            Token::new(TokenType::EOF, String::new()),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn lex_with_strings() {
        let tokens = lex("\"Hello\" \"hELLo\" \"hello\" \"HELLO\"".to_string());
        let expected = vec![
            Token::new(TokenType::STRING, "Hello".to_string()),
            Token::new(TokenType::STRING, "hELLo".to_string()),
            Token::new(TokenType::STRING, "hello".to_string()),
            Token::new(TokenType::STRING, "HELLO".to_string()),
            Token::new(TokenType::EOF, String::new()),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn lex_no_space() {
        let tokens = lex("5+6".to_string());
        let expected = vec![
            Token::new(TokenType::NUMBER, "5".to_string()),
            Token::new(TokenType::Plus, "+".to_string()),
            Token::new(TokenType::NUMBER, "6".to_string()),
            Token::new(TokenType::EOF, String::new()),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn boolean_identifiers() {
        let tokens = lex("true false nil".to_string());
        let expected = vec![
            Token::new(TokenType::TRUE, "true".to_string()),
            Token::new(TokenType::FALSE, "false".to_string()),
            Token::new(TokenType::Nil, "nil".to_string()),
            Token::new(TokenType::EOF, String::new()),
        ];
        assert_eq!(expected, tokens);
    }
}