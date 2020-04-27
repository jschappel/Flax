use crate::lexer::{Token, TokenType};
use std::fmt;
use std::fmt::{ Display };
/** Grammar
 * 
 * Expression       => literal 
 *                   | Binary
 *                   | urnary
 * 
 * urnary           => ('-') expression
 * 
 * Binary           => expression operator expression
 * 
 * Grouping         => "(" expression ")"
 * 
 * Literal          => NUMBER
 * 
 * operator         => '-' '+' '*' '/'
 * 
**/

#[derive(Debug)]
pub enum ExprType {
    L(Literal),
    U(Box<Unary>),
    B(Box<Binary>),
    G(Box<Grouping>),
}

#[derive(Debug)]
pub struct Expr {
    pub expr: ExprType
}

impl Expr {
    
    pub fn new_binary_expr(bin_expr: Binary) -> Expr {
        Expr { expr: ExprType::B(Box::new(bin_expr)) }
    }

    pub fn new_literal_expr(literal: Literal) -> Expr {
        Expr { expr: ExprType::L(literal) }
    }

    pub fn new_unary_expr(unary: Unary) -> Expr {
        Expr { expr: ExprType::U(Box::new(unary)) }
    }

    pub fn new_grouping_expr(grouping: Grouping) -> Expr {
        Expr { expr: ExprType::G(Box::new(grouping)) }
    }
}


#[derive(Debug)]
pub struct Grouping {
    pub expr: Expr
}

impl Grouping {
    pub fn new(expr: Expr) -> Grouping {
        Grouping { expr }
    }
}




#[derive(Debug)]
pub struct Binary {
    pub operator: Token,
    pub left: Expr,
    pub right: Expr,
}

impl Binary {
    pub fn new(token: Token, left: Expr, right: Expr) -> Binary {
        Binary {operator: token, left, right }
    }
}




#[derive(Debug)]
pub struct Literal {
    pub val: String
}

impl Literal {
    pub fn new(val: String) -> Literal {
        Literal { val }
    }
}



#[derive(Debug)]
pub struct Unary {
    pub operator: Token,
    pub expr: Expr,
}

impl Unary {
    pub fn new(operator: Token, expr: Expr) -> Unary {
        Unary { operator, expr }
    }
}





pub trait Data  {
    fn accept<R, V: Interpreter<R>>(&self, visitor: &V) -> R;
}

impl Data for Unary {
    fn accept<R, V: Interpreter<R>>(&self, visitor: &V) -> R {
        visitor.visit_unary(self)
    }
}

impl Data for Literal {
    fn accept<R, V: Interpreter<R>>(&self, visitor: &V) -> R {
        visitor.visit_literal(self)
    }
}

impl Data for Binary {
    fn accept<R, V: Interpreter<R>>(&self, visitor: &V) -> R {
        visitor.visit_binary(self)
    }
}

impl Data for Grouping {
    fn accept<R, V: Interpreter<R>>(&self, visitor: &V) -> R {
        visitor.visit_grouping(self)
    }
}


pub trait Interpreter<R> {

    fn visit_binary(&self, binary: &Binary) -> R;
    fn visit_unary(&self, urnary: &Unary) -> R;
    fn visit_literal(&self, literal: &Literal) -> R;
    fn visit_grouping(&self, grouping: &Grouping) -> R;

}

// pub fn interpret(expression: Expr) -> f64 {
//     match expression.expr {
//         ExprType::B(ref val) => expression.visit_binary(val),
//         ExprType::G(ref val) => expression.visit_grouping(val),
//         ExprType::L(ref val) => expression.visit_literal(val),
//         ExprType::U(ref val) => expression.visit_urnary(val),
//     }
// }

pub fn interpret2(expression: Expr) -> Obj {
    match expression.expr {
        ExprType::B(ref val) => expression.visit_binary(val),
        ExprType::G(ref val) => expression.visit_grouping(val),
        ExprType::L(ref val) => expression.visit_literal(val),
        ExprType::U(ref val) => expression.visit_unary(val),
    }
}

macro_rules! evaluate {
    ($e:expr, $sel:ident) => {
        match &$e {
            ExprType::L(lit) => lit.accept($sel),
            ExprType::B(ref b_expr) => b_expr.accept($sel),
            ExprType::U(ref u_expr) => u_expr.accept($sel),
            ExprType::G(ref g_expr) => g_expr.accept($sel),
        }
    };
}

#[derive(PartialEq, Debug)]
pub enum Obj {
    BOOL(bool),
    STRING(String),
    NUMBER(f64),
    Nil,
}

impl Display for Obj {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        match self {
            Obj::BOOL(val) => write!(f, "{}", val),
            Obj::Nil => write!(f, "nil"),
            Obj::STRING(val) => write!(f, "\"{}\"", val),
            Obj::NUMBER(val) => write!(f, "{}", val),
        }
    }
}



impl Interpreter<f64> for Expr {
    fn visit_binary(&self, binary: &Binary) -> f64 {
        let right: f64 = evaluate!(binary.right.expr, self);
        let left: f64 = evaluate!(binary.left.expr, self);
        
        match binary.operator.token_type {
            TokenType::Minus => left - right,
            TokenType::Plus => left + right,
            TokenType::Star => left * right,
            TokenType::Slash => left / right,
            _ =>  panic!("Invalid token for Binary"),
        }
    }

    fn visit_unary(&self, urnary: &Unary) -> f64 {
        let expr: f64 = evaluate!(urnary.expr.expr, self);

        match urnary.operator.token_type {
            TokenType::Minus => -1.0 * expr,
            _ => panic!("Invalid token for Unary"),
        }
        
    }

    fn visit_literal(&self, literal: &Literal) -> f64 {
        literal.val.parse::<f64>().unwrap()
    }

    fn visit_grouping(&self, grouping: &Grouping) -> f64 {
        evaluate!(grouping.expr.expr, self)
    }
}


impl Interpreter<Obj> for Expr {
    fn visit_binary(&self, binary: &Binary) -> Obj {
        let right: Obj = evaluate!(binary.right.expr, self);
        let left: Obj = evaluate!(binary.left.expr, self);
        
        match binary.operator.token_type {
            TokenType::Minus => check_numbers(left, right, TokenType::Minus),
            TokenType::Plus => check_numbers(left, right, TokenType::Plus),
            TokenType::Star => check_numbers(left, right, TokenType::Star),
            TokenType::Slash => check_numbers(left, right, TokenType::Slash),
            TokenType::PlusPlus => check_strings(left, right),
            TokenType::EqualEqual => determine_equality((left, right), TokenType::EqualEqual),
            TokenType::BangEqual => determine_equality((left, right), TokenType::BangEqual),
            TokenType::Less => determine_int_comparison((left, right), TokenType::Less),
            TokenType::LessEqual => determine_int_comparison((left, right), TokenType::LessEqual),
            TokenType::Greater => determine_int_comparison((left, right), TokenType::Greater),
            TokenType::GreaterEqual => determine_int_comparison((left, right), TokenType::GreaterEqual),
            _ =>  panic!("Invalid token for Binary"),
        }
    }

    fn visit_unary(&self, urnary: &Unary) -> Obj {
        let expr: Obj = evaluate!(urnary.expr.expr, self);

        match urnary.operator.token_type {
            TokenType::Minus => {
                if let Obj::NUMBER(v) = expr {
                   return Obj::NUMBER(-1.0 * v);
                }
                panic!("Invalid unary expression.  Expected Number")
            },
            TokenType::Bang => Obj::BOOL(!is_truthy(expr)),
            _ => panic!("Invalid token for Unary"),
        }
    }

    fn visit_literal(&self, literal: &Literal) -> Obj {        
        if literal.val.parse::<f64>().is_ok() {
            Obj::NUMBER(literal.val.parse::<f64>().unwrap())
        }
        else if literal.val.parse::<bool>().is_ok() {
            Obj::BOOL(literal.val.parse::<bool>().unwrap())
        }
        else if literal.val.parse::<String>().is_ok() {
            let s = literal.val.parse::<String>().unwrap();
            match &s[..] {
                "nil" => Obj::Nil,
                "true" => Obj::BOOL(true),
                "false" => Obj::BOOL(false),
                _ => Obj::STRING(s)
            }
        }
        else {
            panic!("Parsing error with: Literal")
        }
    }

    fn visit_grouping(&self, grouping: &Grouping) -> Obj {
        evaluate!(grouping.expr.expr, self)
    }
}
// TODO: Better error handling
fn check_numbers(left: Obj, right: Obj, op: TokenType) -> Obj {
    if let Obj::NUMBER(v1) = left {
        if let Obj::NUMBER(v2) = right {
            return match op {
                TokenType::Minus => Obj::NUMBER(v1 - v2),
                TokenType::Plus => Obj::NUMBER(v1 + v2),
                TokenType::Star => Obj::NUMBER(v1 * v2),
                TokenType::Slash => Obj::NUMBER(v1 / v2),
                _ => panic!("Error"),
            }
        }      
    }
    panic!("Expected Number for -");
}

// Two cases:
// left and right are strings               =>combine the strings 
// left is a string and right is a int      => combine the string and int into a string
fn check_strings(left: Obj, right: Obj) -> Obj {
    if let Obj::STRING(mut v) = left {
        return match right {
            Obj::STRING(v2) => {
               v.push_str(&v2);
               println!("The string is {}",v);
               Obj::STRING(v)
            },
            Obj::NUMBER(v2) => {
                v.push_str(&v2.to_string());
                Obj::STRING(v)
            },
            _ => panic!("Invalid literal type"),
        }
    }
    else if let Obj::STRING(mut v) = right {
        return match left {
            Obj::STRING(v2) => {
               v.push_str(&v2);
               Obj::STRING(v)
            },
            Obj::NUMBER(v2) => {
                v.push_str(&v2.to_string());
                Obj::STRING(v)
            },
            _ => panic!("Invalid literal type"),
        }
    }
    panic!("Expected two strings or a string and a int")
}

fn determine_equality(pair: (Obj, Obj), operator: TokenType) -> Obj {
    match operator {
        TokenType::EqualEqual => {
             match pair {
                (Obj::BOOL(v), Obj::BOOL(v2)) => Obj::BOOL(v == v2),
                (Obj::Nil, Obj::Nil) => Obj::BOOL(true),
                (Obj::STRING(v), Obj::STRING(v2)) => Obj::BOOL(v == v2),
                (Obj::NUMBER(v), Obj::NUMBER(v2)) => Obj::BOOL(v == v2),
                _ => Obj::BOOL(false),
            }
        },
        TokenType::BangEqual => {
            match pair {
                (Obj::BOOL(v), Obj::BOOL(v2)) => Obj::BOOL(v != v2),
                (Obj::Nil, Obj::Nil) => Obj::BOOL(false),
                (Obj::STRING(v), Obj::STRING(v2)) => Obj::BOOL(v != v2),
                (Obj::NUMBER(v), Obj::NUMBER(v2)) => Obj::BOOL(v != v2),
                _ => Obj::BOOL(true),
            }
        },
        _ => panic!("Invalid token type. Expected '==' or '!='.")
    }
}

fn determine_int_comparison(pair: (Obj, Obj), operator: TokenType) -> Obj {
    match pair {
        (Obj::NUMBER(val), Obj::NUMBER(val2)) => {
            match operator {
                TokenType::Less => Obj::BOOL(val < val2),
                TokenType::LessEqual => Obj::BOOL(val <= val2),
                TokenType::Greater => Obj::BOOL(val > val2),
                TokenType::GreaterEqual => Obj::BOOL(val >= val2),
                _ => panic!("Expected boolean values")
            } 
        }, 
        _ => panic!("Expected integer values"),
    }
}

// Determines if  a value is truthy or falsy
// Important: Flax follows Ruby's rule: everything but False and nil are true
fn is_truthy(value: Obj) -> bool {
    match value {
        Obj::BOOL(false) | Obj::Nil => false,
        _ => true,
    }
}





impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}


impl Display for Grouping {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        write!(f, "(Group {})",self.expr)
    }
}

impl Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        write!(f, "({} {} {})", self.operator, self.left, self.right)
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        write!(f, "{}", self.val)
    }
}

impl Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        write!(f, "({} {})", self.operator, self.expr)
    }
}


impl Display for ExprType {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        match self {
            ExprType::L(lit) => write!(f, "{}", lit),
            ExprType::U(ur) => write!(f, "{}", ur),
            ExprType::B(bi) => write!(f, "{}", bi),
            ExprType::G(grp) => write!(f, "{}", grp),
        }
    }
}
