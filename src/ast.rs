use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
}

pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

#[derive(PartialEq, Eq, Debug)]
pub struct LetStatement {
    pub tok: Token, /* the Let token */
    pub name: Identifier,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Identifier {
    pub tok: Token, /* the Ident token */
    pub value: std::sync::Arc<str>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct ReturnStatement {
    pub tok: Token, /* the Return token */
}

#[derive(PartialEq, Eq, Debug)]
pub struct ExpressionStatement {
    pub tok: Token,
    pub expression: Expression,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Expression {
    Identifier(Identifier),
    Integer(IntegerLiteral),
    Boolean(BooleanLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IfExpression(IfExpression),
}

#[derive(PartialEq, Eq, Debug)]
pub struct IntegerLiteral {
    pub tok: Token,
    pub value: i64,
}

#[derive(PartialEq, Eq, Debug)]
pub struct BooleanLiteral {
    pub tok: Token,
    pub value: bool,
}

#[derive(PartialEq, Eq, Debug)]
pub enum PrefixOperator {
    Bang,
    Minus,
}

#[derive(PartialEq, Eq, Debug)]
pub struct PrefixExpression {
    pub tok: Token,
    pub operator: PrefixOperator,
    pub right: std::rc::Rc<Expression>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum InfixOperator {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,
}

#[derive(PartialEq, Eq, Debug)]
pub struct InfixExpression {
    pub tok: Token,
    pub left: std::rc::Rc<Expression>,
    pub operator: InfixOperator,
    pub right: std::rc::Rc<Expression>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct IfExpression {
    pub tok: Token, /* the If token */
    pub condition: std::rc::Rc<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct BlockStatement {
    pub tok: Token, /* the { token */
    pub statements: Vec<Statement>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        todo!()
    }

    fn string(&self) -> String {
        let mut res = String::new();
        for stmt in self.statements.iter() {
            let s = stmt.string();
            res.push_str(&s);
        }
        res
    }
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::LetStatement(ls) => ls.token_literal(),
            Statement::ReturnStatement(rs) => rs.token_literal(),
            Statement::ExpressionStatement(es) => es.token_literal(),
        }
    }

    fn string(&self) -> String {
        match self {
            Statement::LetStatement(ls) => ls.string(),
            Statement::ReturnStatement(rs) => rs.string(),
            Statement::ExpressionStatement(es) => es.string(),
        }
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        "let".to_owned()
    }

    fn string(&self) -> String {
        let mut res = String::new();
        res.push_str(&self.token_literal());
        res.push(' ');
        res.push_str(&self.name.string());
        res.push_str(" = ");
        res.push(';');
        res
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        if let Token::Ident(v) = &self.tok {
            v.to_string()
        } else {
            panic!("unreachable token type in Identifier")
        }
    }

    fn string(&self) -> String {
        self.token_literal()
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        "return".to_owned()
    }

    fn string(&self) -> String {
        let mut res = String::new();
        res.push_str(&self.token_literal());
        res.push(' ');
        res.push(';');
        res
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        todo!()
    }

    fn string(&self) -> String {
        self.expression.string()
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        todo!()
    }
    fn string(&self) -> String {
        self.value.to_string()
    }
}

impl Node for BooleanLiteral {
    fn token_literal(&self) -> String {
        match self.tok {
            Token::True => "true".to_owned(),
            Token::False => "false".to_owned(),
            _ => panic!("unreachable boolean token literal"),
        }
    }
    fn string(&self) -> String {
        self.token_literal()
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        todo!()
    }
    fn string(&self) -> String {
        let mut res = String::new();
        res.push('(');
        match self.operator {
            PrefixOperator::Bang => res.push('!'),
            PrefixOperator::Minus => res.push('-'),
        }
        res.push_str(&self.right.string());
        res.push(')');
        res
    }
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        todo!()
    }
    fn string(&self) -> String {
        let mut res = String::new();
        res.push('(');
        res.push_str(&self.left.string());
        res.push(' ');
        match self.operator {
            InfixOperator::Plus => res.push('+'),
            InfixOperator::Minus => res.push('-'),
            InfixOperator::Asterisk => res.push('*'),
            InfixOperator::Slash => res.push('/'),
            InfixOperator::Lt => res.push('<'),
            InfixOperator::Gt => res.push('>'),
            InfixOperator::Eq => res.push_str("=="),
            InfixOperator::NotEq => res.push_str("!="),
        }
        res.push(' ');
        res.push_str(&self.right.string());
        res.push(')');
        res
    }
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        "if".to_owned()
    }
    fn string(&self) -> String {
        let mut res = String::new();
        res.push_str("if");
        res.push_str(&self.condition.string());
        res.push(' ');
        res.push_str(&self.consequence.string());
        match &self.alternative {
            Some(alt) => {
                res.push_str("else ");
                res.push_str(&alt.string());
            }
            None => {}
        };
        res
    }
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        "{".to_owned()
    }
    fn string(&self) -> String {
        let mut res = String::new();
        for stmt in self.statements.iter() {
            res.push_str(&stmt.string())
        }
        res
    }
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        todo!()
    }

    fn string(&self) -> String {
        match self {
            Expression::Identifier(i) => i.string(),
            Expression::Integer(i) => i.string(),
            Expression::Boolean(b) => b.string(),
            Expression::PrefixExpression(pe) => pe.string(),
            Expression::InfixExpression(ie) => ie.string(),
            Expression::IfExpression(ife) => ife.string(),
        }
    }
}
