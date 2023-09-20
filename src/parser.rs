use crate::ast::{
    BlockStatement, BooleanLiteral, CallExpression, Expression, ExpressionStatement,
    FunctionLiteral, Identifier, IfExpression, InfixExpression, InfixOperator, IntegerLiteral,
    LetStatement, PrefixExpression, PrefixOperator, Program, ReturnStatement, Statement,
};
use crate::lexer::Lexer;
use crate::token::Token;

pub struct Parser {
    l: Lexer,
    cur: Token,
    peek: Token,
    errors: Vec<String>,
}

#[derive(Eq, PartialEq, PartialOrd, Ord)]
enum Precedence {
    Lowest = 0,
    Equals = 1,
    LessGreater = 2,
    Sum = 3,
    Product = 4,
    Prefix = 5,
    Call = 6,
}

impl Parser {
    pub fn new(mut l: Lexer) -> Self {
        let cur = l.next_token();
        let peek = l.next_token();
        let errors = Vec::new();
        Parser {
            l,
            cur,
            peek,
            errors,
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut res: Vec<Statement> = Vec::new();
        while self.cur != Token::Eof {
            let stmt = self.parse_statement();
            match stmt {
                Some(s) => res.push(s),
                None => {}
            }
            self.next_token();
        }
        Program { statements: res }
    }

    pub fn errors_len(&self) -> usize {
        self.errors.len()
    }

    pub fn get_errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match &self.cur {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let tok = self.cur.clone();
        let name: Identifier;
        if let Token::Ident(v) = self.peek.clone() {
            self.next_token();
            name = Identifier {
                tok: self.cur.clone(),
                value: v.clone(),
            }
        } else {
            let e = format!(
                "expected next token to be Token::Ident, got {:#?} instead",
                self.peek
            );
            self.errors.push(e);
            return None;
        }
        if !self.expect_peek(Token::Assign) {
            return None;
        }
        while !self.cur_token_is(Token::Semicolon) {
            self.next_token();
        }
        Some(Statement::LetStatement(LetStatement { tok, name }))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let tok = self.cur.clone();
        self.next_token();
        while !self.cur_token_is(Token::Semicolon) {
            self.next_token();
        }
        Some(Statement::ReturnStatement(ReturnStatement { tok }))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let tok = self.cur.clone();
        match self.parse_expression(Precedence::Lowest) {
            Some(e) => {
                let res = Some(Statement::ExpressionStatement(ExpressionStatement {
                    tok,
                    expression: e,
                }));
                if self.peek_token_is(&Token::Semicolon) {
                    self.next_token();
                }
                res
            }
            None => None,
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let mut left = match &self.cur {
            Token::Ident(_) => Some(self.parse_identifier()),
            Token::Int(_) => self.parse_integer_literal(),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            Token::True | Token::False => Some(self.parse_boolean_literal()),
            Token::LParen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_literal(),
            _ => {
                let e = format!("no prefix parse fn for {:#?}", self.cur);
                self.errors.push(e);
                None
            }
        };

        while !self.peek_token_is(&Token::Semicolon) && precedence < self.peek_precedence() {
            match &self.peek {
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::Eq
                | Token::NotEq
                | Token::Lt
                | Token::Gt => {
                    self.next_token();
                    let l = match left.clone() {
                        Some(exp) => exp,
                        None => return None,
                    };
                    left = self.parse_infix_expression(l);
                }
                Token::LParen => {
                    self.next_token();
                    let l = match left.clone() {
                        Some(exp) => exp,
                        None => return None,
                    };
                    left = self.parse_call_expression(l);
                }
                _ => return left,
            }
        }
        left
    }

    fn parse_identifier(&mut self) -> Expression {
        if let Token::Ident(v) = &self.cur {
            let tok = self.cur.clone();
            Expression::Identifier(Identifier {
                tok,
                value: v.clone(),
            })
        } else {
            panic!("unreachable");
        }
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        if let Token::Int(v) = &self.cur {
            let tok = self.cur.clone();
            match v.parse::<i64>() {
                Ok(i) => Some(Expression::Integer(IntegerLiteral { tok, value: i })),
                Err(_) => None,
            }
        } else {
            panic!("unreachable");
        }
    }

    fn parse_boolean_literal(&mut self) -> Expression {
        let tok = self.cur.clone();
        let value = self.cur == Token::True;
        Expression::Boolean(BooleanLiteral { tok, value })
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let operator = match self.cur {
            Token::Minus => PrefixOperator::Minus,
            Token::Bang => PrefixOperator::Bang,
            _ => return None,
        };
        let tok = self.cur.clone();
        self.next_token();
        let right = self.parse_expression(Precedence::Prefix);
        match right {
            Some(exp) => Some(Expression::PrefixExpression(PrefixExpression {
                tok,
                operator,
                right: std::rc::Rc::new(exp),
            })),
            None => None,
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let operator = match self.cur {
            Token::Plus => InfixOperator::Plus,
            Token::Minus => InfixOperator::Minus,
            Token::Asterisk => InfixOperator::Asterisk,
            Token::Slash => InfixOperator::Slash,
            Token::Eq => InfixOperator::Eq,
            Token::NotEq => InfixOperator::NotEq,
            Token::Lt => InfixOperator::Lt,
            Token::Gt => InfixOperator::Gt,
            _ => return None,
        };
        let tok = self.cur.clone();
        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence);
        match right {
            Some(exp) => Some(Expression::InfixExpression(InfixExpression {
                tok,
                left: std::rc::Rc::new(left),
                operator,
                right: std::rc::Rc::new(exp),
            })),
            None => None,
        }
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();
        let exp = self.parse_expression(Precedence::Lowest);
        if !self.expect_peek(Token::RParen) {
            return None;
        }
        exp
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        let tok = self.cur.clone();
        if !self.expect_peek(Token::LParen) {
            return None;
        }
        self.next_token();
        let cond_opt = self.parse_expression(Precedence::Lowest);
        let condition = match cond_opt {
            Some(c) => std::rc::Rc::new(c),
            None => return None,
        };
        if !self.expect_peek(Token::RParen) {
            return None;
        }
        if !self.expect_peek(Token::LSquirly) {
            return None;
        }
        let consequence = self.parse_block_statement();
        if self.peek_token_is(&Token::Else) {
            self.next_token();
            if !self.expect_peek(Token::LSquirly) {
                return None;
            }
            let alternative = self.parse_block_statement();
            return Some(Expression::IfExpression(IfExpression {
                tok,
                condition,
                consequence,
                alternative: Some(alternative),
            }));
        }
        Some(Expression::IfExpression(IfExpression {
            tok,
            condition,
            consequence,
            alternative: None,
        }))
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut statements = Vec::new();
        let tok = self.cur.clone();
        self.next_token();
        while !self.cur_token_is(Token::RSquirly) && !self.cur_token_is(Token::Eof) {
            let stmt = self.parse_statement();
            match stmt {
                Some(s) => statements.push(s),
                None => {}
            };
            self.next_token();
        }
        BlockStatement { tok, statements }
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        let tok = self.cur.clone();
        if !self.expect_peek(Token::LParen) {
            return None;
        }
        let parameters_opt = self.parse_function_parameters();
        let parameters = match parameters_opt {
            Some(p) => p,
            None => return None,
        };
        if !self.expect_peek(Token::LSquirly) {
            return None;
        }
        let body = self.parse_block_statement();
        Some(Expression::FunctionLiteral(FunctionLiteral {
            tok,
            parameters,
            body,
        }))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut res = Vec::new();
        if self.peek_token_is(&Token::RParen) {
            self.next_token();
            return Some(res);
        }
        self.next_token();
        let mut ident = match &self.cur {
            Token::Ident(v) => Identifier {
                tok: self.cur.clone(),
                value: v.clone(),
            },
            _ => return None,
        };
        res.push(ident);
        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            ident = match &self.cur {
                Token::Ident(v) => Identifier {
                    tok: self.cur.clone(),
                    value: v.clone(),
                },
                _ => return None,
            };
            res.push(ident);
        }
        if !self.expect_peek(Token::RParen) {
            return None;
        }
        Some(res)
    }

    fn parse_call_expression(&mut self, func: Expression) -> Option<Expression> {
        let tok = self.cur.clone();
        let function = std::rc::Rc::new(func);
        match self.parse_call_arguments() {
            Some(arguments) => Some(Expression::CallExpression(CallExpression {
                tok,
                function,
                arguments,
            })),
            None => None,
        }
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Expression>> {
        let mut res: Vec<Expression> = Vec::new();
        if self.peek_token_is(&Token::RParen) {
            self.next_token();
            return Some(res);
        }
        self.next_token();
        match self.parse_expression(Precedence::Lowest) {
            Some(e) => res.push(e),
            None => return None,
        }
        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            match self.parse_expression(Precedence::Lowest) {
                Some(e) => res.push(e),
                None => return None,
            };
        }
        if !self.expect_peek(Token::RParen) {
            return None;
        }
        Some(res)
    }

    fn next_token(&mut self) {
        self.cur = self.peek.clone();
        self.peek = self.l.next_token();
    }

    fn cur_token_is(&self, tok: Token) -> bool {
        self.cur == tok
    }

    fn peek_token_is(&self, tok: &Token) -> bool {
        self.peek == *tok
    }

    fn expect_peek(&mut self, tok: Token) -> bool {
        if !self.peek_token_is(&tok) {
            self.peek_error(&tok);
            false
        } else {
            self.next_token();
            true
        }
    }

    fn peek_error(&mut self, tok: &Token) {
        let str = format!(
            "expected next token to be {:#?}, got {:#?} instead",
            tok, self.peek
        );
        self.errors.push(str);
    }

    fn peek_precedence(&self) -> Precedence {
        match &self.peek {
            Token::Eq => Precedence::Equals,
            Token::NotEq => Precedence::Equals,
            Token::Lt => Precedence::LessGreater,
            Token::Gt => Precedence::LessGreater,
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Asterisk => Precedence::Product,
            Token::Slash => Precedence::Product,
            Token::LParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    fn cur_precedence(&self) -> Precedence {
        match &self.cur {
            Token::Eq => Precedence::Equals,
            Token::NotEq => Precedence::Equals,
            Token::Lt => Precedence::LessGreater,
            Token::Gt => Precedence::LessGreater,
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Asterisk => Precedence::Product,
            Token::Slash => Precedence::Product,
            Token::LParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::{Expression, InfixOperator, Node, PrefixOperator, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    struct BoolTest {
        input: &'static str,
        exp: bool,
    }

    struct PrefixIntTest {
        input: &'static str,
        oper: PrefixOperator,
        int_val: i64,
    }

    struct PrefixBoolTest {
        input: &'static str,
        oper: PrefixOperator,
        bool_val: bool,
    }

    struct InfixIntTest {
        input: &'static str,
        lval: i64,
        oper: InfixOperator,
        rval: i64,
    }

    struct InfixBoolTest {
        input: &'static str,
        lval: bool,
        oper: InfixOperator,
        rval: bool,
    }

    struct PrecedenceTest {
        input: &'static str,
        exp: &'static str,
    }

    fn test_let_statement(stmt: &Statement, name: &str) {
        if let Statement::LetStatement(ls) = stmt {
            let lit = ls.token_literal();
            assert_eq!(lit, "let");
            assert_eq!(ls.name.value.to_string(), name.to_owned());
            assert_eq!(ls.name.token_literal(), name.to_owned());
        } else {
            eprintln!("{:#?} is not a let statement", stmt);
            assert!(false);
        }
    }

    fn test_integer_exp(exp: &Expression, exp_int: i64) {
        if let Expression::Integer(il) = exp {
            assert_eq!(il.value, exp_int);
        } else {
            eprintln!("{:#?} is not an integer literal", exp);
            assert!(false);
        }
    }

    fn test_boolean_exp(exp: &Expression, exp_bool: bool) {
        if let Expression::Boolean(bl) = exp {
            assert_eq!(bl.value, exp_bool);
        } else {
            eprintln!("{:#?} is not a boolean literal", exp);
            assert!(false);
        }
    }

    fn check_errors(p: &Parser) {
        if p.errors_len() > 0 {
            for e in p.get_errors() {
                println!("{}", e);
            }
            panic!("parser had errors")
        }
    }

    fn test_ident(exp: &Expression, name: &str) {
        if let Expression::Identifier(i) = exp {
            assert_eq!(i.value.to_string(), name.to_owned());
        } else {
            panic!("{:#?} is not an ident exp", exp);
        }
    }

    fn test_ident_infix_exp(exp: &Expression, lname: &str, rname: &str, oper: InfixOperator) {
        if let Expression::InfixExpression(ie) = exp {
            test_ident(&ie.left, lname);
            test_ident(&ie.right, rname);
            assert_eq!(ie.operator, oper);
        } else {
            panic!("{:#?} is not an infix expression", exp);
        }
    }

    fn test_int_infix_exp(exp: &Expression, lval: i64, rval: i64, oper: InfixOperator) {
        if let Expression::InfixExpression(ie) = exp {
            test_integer_exp(&ie.left, lval);
            test_integer_exp(&ie.right, rval);
            assert_eq!(ie.operator, oper);
        } else {
            panic!("{:#?} is not an infix expression", exp);
        }
    }

    #[test]
    fn test_let_statements() {
        let input = "let x = 5;
        let y = 10;
        let foobar = 838383;";
        let l = Lexer::new(&input);
        let mut p = Parser::new(l);
        let program = p.parse();
        check_errors(&p);
        let exps = vec!["x", "y", "foobar"];
        assert_eq!(program.statements.len(), 3);
        for (i, exp) in exps.iter().enumerate() {
            let stmt = &program.statements[i];
            test_let_statement(&stmt, exp);
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "return 5;
        return 10;
        return 993322;";
        let l = Lexer::new(&input);
        let mut p = Parser::new(l);
        let program = p.parse();
        check_errors(&p);
        assert_eq!(program.statements.len(), 3);
        for stmt in program.statements.iter() {
            if let Statement::ReturnStatement(rs) = stmt {
                assert_eq!(rs.token_literal(), "return".to_string());
            } else {
                let s = format!("{:#?} is not a return statement", stmt);
                panic!("{}", s);
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar";
        let l = Lexer::new(&input);
        let mut p = Parser::new(l);
        let program = p.parse();
        check_errors(&p);
        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        if let Statement::ExpressionStatement(es) = stmt {
            if let Expression::Identifier(i) = &es.expression {
                assert_eq!(i.value.to_string(), "foobar".to_string());
            } else {
                let s = format!("{:#?} is not an identifier expression", es.expression);
                panic!("{}", s);
            }
        } else {
            let s = format!("{:#?} is not an expression statement", stmt);
            panic!("{}", s);
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let l = Lexer::new(&input);
        let mut p = Parser::new(l);
        let program = p.parse();
        check_errors(&p);
        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        if let Statement::ExpressionStatement(es) = stmt {
            if let Expression::Integer(il) = &es.expression {
                assert_eq!(il.value, 5);
            } else {
                let s = format!("{:#?} is not an integer literal expression", es.expression);
                panic!("{}", s);
            }
        } else {
            let s = format!("{:#?} is not an expression statement", stmt);
            panic!("{}", s);
        }
    }

    #[test]
    fn test_prefix_expressoins() {
        let prefix_int_tests = vec![
            PrefixIntTest {
                input: "!5;",
                oper: PrefixOperator::Bang,
                int_val: 5,
            },
            PrefixIntTest {
                input: "-15;",
                oper: PrefixOperator::Minus,
                int_val: 15,
            },
        ];
        let prefix_bool_tests = vec![
            PrefixBoolTest {
                input: "!true;",
                oper: PrefixOperator::Bang,
                bool_val: true,
            },
            PrefixBoolTest {
                input: "!false;",
                oper: PrefixOperator::Bang,
                bool_val: false,
            },
        ];
        for pt in prefix_int_tests.iter() {
            let l = Lexer::new(pt.input);
            let mut p = Parser::new(l);
            let program = p.parse();
            check_errors(&p);
            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            if let Statement::ExpressionStatement(es) = stmt {
                if let Expression::PrefixExpression(pe) = &es.expression {
                    assert_eq!(pe.operator, pt.oper);
                    test_integer_exp(&pe.right, pt.int_val);
                } else {
                    let s = format!("{:#?} is not a prefix expression", es.expression);
                    panic!("{}", s);
                }
            } else {
                let s = format!("{:#?} is not an expression statement", stmt);
                panic!("{}", s);
            }
        }

        for pt in prefix_bool_tests.iter() {
            let l = Lexer::new(pt.input);
            let mut p = Parser::new(l);
            let program = p.parse();
            check_errors(&p);
            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            if let Statement::ExpressionStatement(es) = stmt {
                if let Expression::PrefixExpression(pe) = &es.expression {
                    assert_eq!(pe.operator, pt.oper);
                    test_boolean_exp(&pe.right, pt.bool_val);
                } else {
                    let s = format!("{:#?} is not a prefix expression", es.expression);
                    panic!("{}", s);
                }
            } else {
                let s = format!("{:#?} is not an expression statement", stmt);
                panic!("{}", s);
            }
        }
    }

    #[test]
    fn test_infix_expressions() {
        let int_tests = vec![
            InfixIntTest {
                input: "5 + 5",
                lval: 5,
                oper: InfixOperator::Plus,
                rval: 5,
            },
            InfixIntTest {
                input: "5 - 5",
                lval: 5,
                oper: InfixOperator::Minus,
                rval: 5,
            },
            InfixIntTest {
                input: "5 * 5",
                lval: 5,
                oper: InfixOperator::Asterisk,
                rval: 5,
            },
            InfixIntTest {
                input: "5 / 5",
                lval: 5,
                oper: InfixOperator::Slash,
                rval: 5,
            },
            InfixIntTest {
                input: "5 > 5",
                lval: 5,
                oper: InfixOperator::Gt,
                rval: 5,
            },
            InfixIntTest {
                input: "5 < 5",
                lval: 5,
                oper: InfixOperator::Lt,
                rval: 5,
            },
            InfixIntTest {
                input: "5 == 5",
                lval: 5,
                oper: InfixOperator::Eq,
                rval: 5,
            },
            InfixIntTest {
                input: "5 != 5",
                lval: 5,
                oper: InfixOperator::NotEq,
                rval: 5,
            },
        ];

        let bool_tests = vec![
            InfixBoolTest {
                input: "true == true",
                lval: true,
                oper: InfixOperator::Eq,
                rval: true,
            },
            InfixBoolTest {
                input: "true != false",
                lval: true,
                oper: InfixOperator::NotEq,
                rval: false,
            },
            InfixBoolTest {
                input: "false == false",
                lval: false,
                oper: InfixOperator::Eq,
                rval: false,
            },
        ];

        for it in int_tests.iter() {
            let l = Lexer::new(it.input);
            let mut p = Parser::new(l);
            let program = p.parse();
            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            if let Statement::ExpressionStatement(es) = stmt {
                if let Expression::InfixExpression(ie) = &es.expression {
                    assert_eq!(ie.operator, it.oper);
                    test_integer_exp(&ie.left, it.lval);
                    test_integer_exp(&ie.right, it.rval);
                } else {
                    let s = format!("{:#?} is not a prefix expression", es.expression);
                    panic!("{}", s);
                }
            } else {
                let s = format!("{:#?} is not an expression statement", stmt);
                panic!("{}", s);
            }
        }

        for it in bool_tests.iter() {
            let l = Lexer::new(it.input);
            let mut p = Parser::new(l);
            let program = p.parse();
            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            if let Statement::ExpressionStatement(es) = stmt {
                if let Expression::InfixExpression(ie) = &es.expression {
                    assert_eq!(ie.operator, it.oper);
                    test_boolean_exp(&ie.left, it.lval);
                    test_boolean_exp(&ie.right, it.rval);
                } else {
                    let s = format!("{:#?} is not a prefix expression", es.expression);
                    panic!("{}", s);
                }
            } else {
                let s = format!("{:#?} is not an expression statement", stmt);
                panic!("{}", s);
            }
        }
    }

    #[test]
    fn operator_precedence() {
        let tests = vec![
            PrecedenceTest {
                input: "-a * b",
                exp: "((-a) * b)",
            },
            PrecedenceTest {
                input: "!-a",
                exp: "(!(-a))",
            },
            PrecedenceTest {
                input: "a + b + c",
                exp: "((a + b) + c)",
            },
            PrecedenceTest {
                input: "a + b - c",
                exp: "((a + b) - c)",
            },
            PrecedenceTest {
                input: "a * b * c",
                exp: "((a * b) * c)",
            },
            PrecedenceTest {
                input: "a * b / c",
                exp: "((a * b) / c)",
            },
            PrecedenceTest {
                input: "a + b / c",
                exp: "(a + (b / c))",
            },
            PrecedenceTest {
                input: "a + b * c + d / e - f",
                exp: "(((a + (b * c)) + (d / e)) - f)",
            },
            PrecedenceTest {
                input: "3 + 4; -5 * 5",
                exp: "(3 + 4)((-5) * 5)",
            },
            PrecedenceTest {
                input: "5 > 4 == 3 < 4",
                exp: "((5 > 4) == (3 < 4))",
            },
            PrecedenceTest {
                input: "5 < 4 != 3 > 4",
                exp: "((5 < 4) != (3 > 4))",
            },
            PrecedenceTest {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
                exp: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            },
            PrecedenceTest {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
                exp: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            },
            PrecedenceTest {
                input: "true",
                exp: "true",
            },
            PrecedenceTest {
                input: "false",
                exp: "false",
            },
            PrecedenceTest {
                input: "3 > 5 == false",
                exp: "((3 > 5) == false)",
            },
            PrecedenceTest {
                input: "3 < 5 == true",
                exp: "((3 < 5) == true)",
            },
            PrecedenceTest {
                input: "1 + (2 + 3) + 4",
                exp: "((1 + (2 + 3)) + 4)",
            },
            PrecedenceTest {
                input: "(5 + 5) * 2",
                exp: "((5 + 5) * 2)",
            },
            PrecedenceTest {
                input: "2 / (5 + 5)",
                exp: "(2 / (5 + 5))",
            },
            PrecedenceTest {
                input: "-(5 + 5)",
                exp: "(-(5 + 5))",
            },
            PrecedenceTest {
                input: "!(true == true)",
                exp: "(!(true == true))",
            },
            PrecedenceTest {
                input: "a + add(b * c) + d",
                exp: "((a + add((b * c))) + d)",
            },
            PrecedenceTest {
                input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                exp: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            },
            PrecedenceTest {
                input: "add(a + b + c * d / f + g)",
                exp: "add((((a + b) + ((c * d) / f)) + g))",
            },
        ];

        for t in tests.iter() {
            let l = Lexer::new(t.input);
            let mut p = Parser::new(l);
            let program = p.parse();
            let s = program.string();
            assert_eq!(s, t.exp);
        }
    }

    #[test]
    fn test_boolean_literal() {
        let tests = vec![
            BoolTest {
                input: "true",
                exp: true,
            },
            BoolTest {
                input: "false",
                exp: false,
            },
        ];
        for t in tests.iter() {
            let l = Lexer::new(t.input);
            let mut p = Parser::new(l);
            let program = p.parse();
            check_errors(&p);
            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            if let Statement::ExpressionStatement(es) = stmt {
                test_boolean_exp(&es.expression, t.exp);
            } else {
                let s = format!("{:#?} is not an expression statement", stmt);
                panic!("{}", s);
            }
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let l = Lexer::new(&input);
        let mut p = Parser::new(l);
        check_errors(&p);
        let program = p.parse();
        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        if let Statement::ExpressionStatement(es) = stmt {
            if let Expression::IfExpression(ife) = &es.expression {
                test_ident_infix_exp(&ife.condition, "x", "y", InfixOperator::Lt);
                let consequence = &ife.consequence;
                assert_eq!(consequence.statements.len(), 1);
                let cons_stmt = &consequence.statements[0];
                if let Statement::ExpressionStatement(cons_es) = cons_stmt {
                    test_ident(&cons_es.expression, "x");
                } else {
                    let s = format!("{:#?} is not an expression statement", cons_stmt);
                    panic!("{}", s);
                }
                assert_eq!(ife.alternative, None);
            } else {
                let s = format!("{:#?} is not an if expression", es);
                panic!("{}", s);
            }
        } else {
            let s = format!("{:#?} is not an expression statement", stmt);
            panic!("{}", s);
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        let l = Lexer::new(&input);
        let mut p = Parser::new(l);
        check_errors(&p);
        let program = p.parse();
        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        if let Statement::ExpressionStatement(es) = stmt {
            if let Expression::IfExpression(ife) = &es.expression {
                test_ident_infix_exp(&ife.condition, "x", "y", InfixOperator::Lt);
                let consequence = &ife.consequence;
                assert_eq!(consequence.statements.len(), 1);
                let cons_stmt = &consequence.statements[0];
                if let Statement::ExpressionStatement(cons_es) = cons_stmt {
                    test_ident(&cons_es.expression, "x");
                } else {
                    let s = format!("{:#?} is not an expression statement", cons_stmt);
                    panic!("{}", s);
                }
                let alternative = match &ife.alternative {
                    Some(a) => a,
                    None => panic!("expected if else to have alternative"),
                };
                assert_eq!(alternative.statements.len(), 1);
                let alt_stmt = &alternative.statements[0];
                if let Statement::ExpressionStatement(alt_es) = alt_stmt {
                    test_ident(&alt_es.expression, "y");
                } else {
                    let s = format!("{:#?} is not an expression statement", alt_stmt);
                    panic!("{}", s);
                }
            } else {
                let s = format!("{:#?} is not an if expression", es);
                panic!("{}", s);
            }
        } else {
            let s = format!("{:#?} is not an expression statement", stmt);
            panic!("{}", s);
        }
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) { x + y; }";
        let l = Lexer::new(&input);
        let mut p = Parser::new(l);
        let program = p.parse();
        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        if let Statement::ExpressionStatement(es) = stmt {
            if let Expression::FunctionLiteral(fne) = &es.expression {
                assert_eq!(fne.parameters.len(), 2);
                let ident1 = &fne.parameters[0];
                let ident2 = &fne.parameters[1];
                assert_eq!(ident1.value.to_string(), "x".to_owned());
                assert_eq!(ident2.value.to_string(), "y".to_owned());
                assert_eq!(fne.body.statements.len(), 1);
                let body_stmt = &fne.body.statements[0];
                if let Statement::ExpressionStatement(es) = body_stmt {
                    test_ident_infix_exp(&es.expression, "x", "y", InfixOperator::Plus);
                } else {
                    let s = format!("{:#?} is not an expression statement", stmt);
                    panic!("{}", s);
                }
            } else {
                panic!("{:#?} is not a function literal", es.expression);
            }
        } else {
            let s = format!("{:#?} is not an expression statement", stmt);
            panic!("{}", s);
        }
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let l = Lexer::new(&input);
        let mut p = Parser::new(l);
        let program = p.parse();
        check_errors(&p);
        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        if let Statement::ExpressionStatement(es) = stmt {
            if let Expression::CallExpression(call) = &es.expression {
                test_ident(&call.function, "add");
                assert_eq!(call.arguments.len(), 3);
                let a1 = &call.arguments[0];
                let a2 = &call.arguments[1];
                let a3 = &call.arguments[2];
                test_integer_exp(&a1, 1);
                test_int_infix_exp(&a2, 2, 3, InfixOperator::Asterisk);
                test_int_infix_exp(&a3, 4, 5, InfixOperator::Plus);
            } else {
                let s = format!("{:#?} is not a call expressin", es.expression);
                panic!("{}", s);
            }
        } else {
            let s = format!("{:#?} is not an expression statement", stmt);
            panic!("{}", s);
        }
    }
}
