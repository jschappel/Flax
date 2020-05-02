use crate::lexer::{Token};
use std::fmt;
use std::fmt::{ Display };

/// SEE README.md for core grammar



/// A Statement does not evaluate a value, it instead creates  a side effect
/// A Statement can be one of the following
/// 1) PrintStmt: evaluates an expression and prints it to the console (TEMPORARY)
/// 2) ExprStmt: evaluates an expression
pub enum Stmt {
    PrintStmt(Expr),
    ExprStmt(Expr),
}






/// An Expression can be one of the below types
/// L: Literal
/// U: Unary
/// B: Binary
/// G: Grouping
#[derive(Debug)]
pub enum Expr {
    L(Literal),
    U(Box<Unary>),
    B(Box<Binary>),
    G(Box<Grouping>),
}

impl Expr {
    pub fn new_literal(val: String) -> Expr {
        Expr::L(Literal::new(val))
    }

    pub fn new_unary(op: Token, expr: Expr) -> Expr {
        Expr::U(Box::new(Unary::new(op, expr)))
    }

    pub fn new_binary(left: Expr, op: Token, right: Expr) -> Expr {
        Expr::B(Box::new(Binary::new(op, left, right)))
    }

    pub fn new_grouping(expr: Expr) -> Expr {
        Expr::G(Box::new(Grouping::new(expr)))
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
        match self {
            Expr::L(lit) => write!(f, "{}", lit),
            Expr::U(ur) => write!(f, "{}", ur),
            Expr::B(bi) => write!(f, "{}", bi),
            Expr::G(grp) => write!(f, "{}", grp),
        }
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

impl Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        match self {
            Stmt::ExprStmt(expr) => write!(f, "{}", expr),
            Stmt::PrintStmt(expr) => write!(f, "{}", expr),
        }
    }
}



