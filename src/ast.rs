use crate::lexer::{Token};
use std::fmt;
use std::fmt::{ Display };

/// SEE README.md for core grammar


/// An Expression can be one of the below types
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







// Implement Display for each struct in the Abstract Syntax Tree so we can debug the tree if needed
impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}


impl Display for Grouping {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        write!(f, "(Grp {})",self.expr)
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
