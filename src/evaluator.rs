use crate::ast::{
    Expression, ExpressionStatement, PrefixExpression, PrefixOperator, Program, Statement,
};
use crate::object::Object;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

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
        Expression::Integer(val) => Some(Object::Integer(val.value)),
        Expression::Boolean(val) => Some(native_bool_to_bool_object(val.value)),
        Expression::PrefixExpression(pe) => {
            let right = match eval_expression(&pe.right) {
                Some(val) => val,
                None => return None,
            };
            eval_prefix_expression(&pe, &right)
        }
        _ => None,
    }
}

fn eval_prefix_expression(pe: &PrefixExpression, right: &Object) -> Option<Object> {
    match pe.operator {
        PrefixOperator::Bang => Some(eval_bang_operator(right)),
        PrefixOperator::Minus => Some(eval_minus_operator(right)),
    }
}

fn eval_bang_operator(right: &Object) -> Object {
    match right {
        Object::Boolean(v) => native_bool_to_bool_object(!*v),
        Object::Null => TRUE,
        _ => FALSE,
    }
}

fn eval_minus_operator(right: &Object) -> Object {
    match right {
        Object::Integer(v) => Object::Integer(-v),
        _ => NULL,
    }
}

fn native_bool_to_bool_object(input: bool) -> Object {
    if input {
        TRUE
    } else {
        FALSE
    }
}

#[cfg(test)]
mod test {
    use crate::{evaluator::eval, lexer::Lexer, object::Object, parser::Parser};

    struct IntTest {
        input: &'static str,
        exp: i64,
    }

    struct BoolTest {
        input: &'static str,
        exp: bool,
    }

    fn test_eval(input: &str) -> Option<Object> {
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

    fn test_bool_object(obj: &Object, exp: bool) {
        if let Object::Boolean(v) = obj {
            assert_eq!(*v, exp);
        } else {
            panic!("{:#?} is not a boolean object", obj);
        }
    }

    #[test]
    fn test_eval_int_expression() {
        let tests = vec![
            IntTest { input: "5", exp: 5 },
            IntTest {
                input: "10",
                exp: 10,
            },
            IntTest {
                input: "-5",
                exp: -5,
            },
            IntTest {
                input: "-10",
                exp: -10,
            },
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

    #[test]
    fn test_eval_bool_expression() {
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

        for test in tests.iter() {
            let obj_opt = test_eval(test.input);
            if let Some(obj) = obj_opt {
                test_bool_object(&obj, test.exp);
            } else {
                panic!("evaluator returned None");
            }
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            BoolTest {
                input: "!true",
                exp: false,
            },
            BoolTest {
                input: "!false",
                exp: true,
            },
            BoolTest {
                input: "!5",
                exp: false,
            },
            BoolTest {
                input: "!!true",
                exp: true,
            },
            BoolTest {
                input: "!!false",
                exp: false,
            },
            BoolTest {
                input: "!!5",
                exp: true,
            },
        ];

        for test in tests.iter() {
            let obj_opt = test_eval(test.input);
            if let Some(obj) = obj_opt {
                test_bool_object(&obj, test.exp);
            } else {
                panic!("evaluator returned None");
            }
        }
    }
}
