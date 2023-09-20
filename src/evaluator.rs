use crate::object::Object;
use crate::ast::{Program, Statement, ExpressionStatement, Expression};

pub fn eval(program: &Program) -> Option<Object> {
    eval_statements(&program.statements)
}

fn eval_statements(statements: &Vec<Statement>) -> Option<Object> {
    let mut obj: Option<Object> = None;
    for stmt in statements {
        obj = eval_statement(stmt);
    }
    obj
}

fn eval_statement(statment: &Statement) -> Option<Object> {
    match statment {
        Statement::ExpressionStatement(es) => eval_expression_statement(es),
        _ => None,
    }
}

fn eval_expression_statement(es: &ExpressionStatement) -> Option<Object> {
    eval_expression(&es.expression)
}

fn eval_expression(e: &Expression) -> Option<Object> {
    match e {
        Expression::Integer(val) => {
            Some(Object::Integer(val.value))
        },
        _ => None,
    }
}

#[cfg(test)]
mod test {
    use crate::{lexer::Lexer, parser::Parser, object::Object, evaluator::eval};

    struct IntTest {
        input: &'static str,
        exp: i64,
    }

    fn test_eval(input: &str) -> Option<Object>{
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse();
        eval(&program)
    }

    fn test_int_object(obj: &Object, exp: i64) {
        if let Object::Integer(v) = obj {
            assert_eq!(*v, exp);
        } else {
            panic!("{:#?} is not an integer object", obj);
        }
    }

    #[test]
    fn test_eval_int_expression() {
        let tests = vec![
            IntTest { input: "5", exp: 5 },
            IntTest { input: "10", exp: 10 },
        ];

        for test in tests.iter() {
            let obj_opt = test_eval(test.input);
            if let Some(obj) = obj_opt {
                test_int_object(&obj, test.exp);
            } else {
                panic!("evaluator returned None");
            }
        }
    }
}
