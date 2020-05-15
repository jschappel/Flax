use crate::lexer::{Token};
use std::fmt;
use std::fmt::{ Display };

/// SEE README.md for core grammar



/// A Statement does not evaluate a value, it instead creates  a side effect
/// A Statement can be one of the following
/// 1) PrintStmt: evaluates an expression and prints it to the console (TEMPORARY)
/// 2) ExprStmt: evaluates an expression
/// 3) VarDecl: Variable declaration
/// 4) Block: Block statement 
#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    ExprStmt(Expr),
    IfStmt(Box<IfStatement>),
    VarDecl(Token, Option<Expr>),
    Block(Box<Vec<Stmt>>),
    WhileStmt(Expr, Box<Stmt>),
    FuncStmt(Box<Function>),
    ReturnStmt(Box<Return>),
    Break,
}

impl Stmt {
    pub fn new_block(statements: Vec<Stmt>) -> Stmt {
        Stmt::Block(Box::new(statements))
    }

    pub fn new_if(conditional: Expr, then_block: Stmt, else_block: Option<Stmt>) -> Stmt {
        Stmt::IfStmt(Box::new(IfStatement { conditional, then_block, else_block }))  
    }
    
    pub fn new_while(condition: Expr, block: Stmt) -> Stmt {
        Stmt::WhileStmt(condition, Box::new(block))
    }

    pub fn new_function(name: Token, params: Vec<Token>, body: Vec<Stmt>) -> Stmt {
        Stmt::FuncStmt(Box::new(Function { name, params, body }))
    }

    pub fn new_return(tok: Token, expr: Option<Expr>) -> Stmt {
        Stmt::ReturnStmt(Box::new(Return { tok, expr }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStatement {
    pub conditional: Expr,
    pub then_block: Stmt,
    pub else_block: Option<Stmt>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Function {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Return {
    pub tok: Token,
    pub expr: Option<Expr>,
}


/// An Expression can be one of the below types
/// L: Literal
/// U: Unary
/// B: Binary
/// G: Grouping
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    L(Literal),
    U(Box<Unary>),
    B(Box<Binary>),
    G(Box<Grouping>),
    C(Box<Conditional>),
    V(Token),
    A(Token, Box<Expr>),
    Log(Box<Logical>),
    Cal(Box<Call>),
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

    pub fn new_variable(token: Token) -> Expr {
        Expr::V(token)
    }

    pub fn new_assignment(token: Token, expr: Expr) -> Expr {
        Expr::A(token, Box::new(expr))
    }

    pub fn new_conditional(conditional: Expr, then_expr: Expr, else_expr: Expr, line: u64) -> Expr {
        Expr::C(Box::new(Conditional::new(conditional, then_expr, else_expr, line)))
    }

    pub fn new_logical(tok: Token, left: Expr, right: Expr) -> Expr {
        Expr::Log(Box::new(Logical { tok, left, right }))
    }

    pub fn new_call(callee: Expr, tok: Token, args: Vec<Expr>) -> Expr {
        Expr::Cal(Box::new(Call { callee, tok, args }))
    }
}


#[derive(Debug, PartialEq, Clone)]
pub struct Grouping {
    pub expr: Expr
}

impl Grouping {
    pub fn new(expr: Expr) -> Grouping {
        Grouping { expr }
    }
}




#[derive(Debug, PartialEq, Clone)]
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




#[derive(Debug, PartialEq, Clone)]
pub struct Literal {
    pub val: String
}

impl Literal {
    pub fn new(val: String) -> Literal {
        Literal { val }
    }
}



#[derive(Debug, PartialEq, Clone)]
pub struct Unary {
    pub operator: Token,
    pub expr: Expr,
}

impl Unary {
    pub fn new(operator: Token, expr: Expr) -> Unary {
        Unary { operator, expr }
    }
}



#[derive(Debug, PartialEq, Clone)]
pub struct Conditional {
    pub cond: Expr,
    pub line_num: u64,
    pub then_expr: Expr,
    pub else_expr: Expr,
}

impl Conditional {
    pub fn new(cond: Expr, then_expr: Expr, else_expr: Expr, line_num: u64) -> Conditional {
        Conditional { cond, then_expr, else_expr, line_num }
    }
}


#[derive(Debug, PartialEq, Clone)]
pub struct Logical {
    pub tok: Token,
    pub left: Expr,
    pub right: Expr,
}


#[derive(Debug, PartialEq, Clone)]
pub struct Call {
    pub callee: Expr,
    pub args: Vec<Expr>,
    pub tok: Token,
}






// Implement Display for each struct in the Abstract Syntax Tree so we can debug the tree if needed
impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        match self {
            Expr::L(lit) => write!(f, "{}", lit),
            Expr::U(ur) => write!(f, "{}", ur),
            Expr::B(bi) => write!(f, "{}", bi),
            Expr::G(grp) => write!(f, "{}", grp),
            Expr::V(tok) => write!(f, "{}", tok.lexeme),
            Expr::A(_, expr) => write!(f, "{}", expr),
            Expr::C(cond) => write!(f, "{}", cond),
            Expr::Log(logical) => write!(f, "{}", logical),
            Expr::Cal(_) => write!(f, "{}", self)
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

impl Display for Conditional {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        write!(f, "({} ? {} : {})", self.cond, self.then_expr, self.else_expr)
    }
}

impl Display for Logical {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        write!(f, "({} {} {})", self.tok.lexeme, self.left, self.right)
    }
}

impl Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        write!(f, "({} ({:?}))", self.callee, self.args)
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        match self {
            Stmt::ExprStmt(expr) => write!(f, "{}", expr),
            Stmt::VarDecl(name, expr) => {
                match expr {
                    Some(e) => write!(f, "(name = {})", e),
                    None => write!(f, "({})", name.lexeme),
                }
            },
            Stmt::Block(_) => write!(f, "Placeholder for block"),
            Stmt::IfStmt(_) => write!(f, "Placeholder for block"),
            Stmt::WhileStmt(_,_) => write!(f, "Placeholder for while"),
            Stmt::Break => write!(f, "Placeholder for while"),
            Stmt::FuncStmt(_) => write!(f, "Placeholder for func stmt"),
            Stmt::ReturnStmt(_) => write!(f, "Placeholder for return stmt"),
        }
    }
}



