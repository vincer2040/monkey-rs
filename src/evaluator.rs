use std::ops::Deref;

use crate::ast::{
    Expression, ExpressionStatement, HashLiteral, IfExpression, InfixOperator, PrefixExpression,
    PrefixOperator, Program, Statement,
};
use crate::builtins::{first, last, len, push, rest};
use crate::environment::Environment;
use crate::object::{Array, Builtin, Function, Hash, Object, ObjectTrait, ObjectType};

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);
pub const NULL: Object = Object::Null;

const LEN: Object = Object::Builtin(Builtin { func: len });
const FIRST: Object = Object::Builtin(Builtin { func: first });
const LAST: Object = Object::Builtin(Builtin { func: last });
const REST: Object = Object::Builtin(Builtin { func: rest });
const PUSH: Object = Object::Builtin(Builtin { func: push });

pub fn eval(program: &Program, env: &mut Environment) -> Option<Object> {
    eval_statements(&program.statements, env)
}

fn eval_statements(statements: &Vec<Statement>, env: &mut Environment) -> Option<Object> {
    let mut obj: Option<Object> = None;
    for stmt in statements {
        obj = eval_statement(stmt, env);
        if let Some(o) = obj.clone() {
            match o {
                Object::Return(ret) => {
                    let x = ret.deref().to_owned();
                    return Some(x);
                }
                Object::Error(_) => return Some(o),
                _ => {}
            }
        }
    }
    obj
}

fn eval_statement(statement: &Statement, env: &mut Environment) -> Option<Object> {
    match statement {
        Statement::LetStatement(ls) => {
            let val = eval_expression(&ls.value, env);
            if let Some(exp) = val.clone() {
                if exp.type_val() == ObjectType::Error {
                    return val;
                } else {
                    env.set(ls.name.value.clone(), exp);
                    return None;
                }
            } else {
                return None;
            }
        }
        Statement::ReturnStatement(rs) => {
            let return_value = match eval_expression(&rs.value, env) {
                Some(v) => v,
                None => return None,
            };
            if return_value.type_val() == ObjectType::Error {
                return Some(return_value);
            }
            Some(Object::Return(std::boxed::Box::new(return_value)))
        }
        Statement::ExpressionStatement(es) => eval_expression_statement(es, env),
    }
}

fn eval_expression_statement(es: &ExpressionStatement, env: &mut Environment) -> Option<Object> {
    eval_expression(&es.expression, env)
}

fn eval_expression(e: &Expression, env: &mut Environment) -> Option<Object> {
    match e {
        Expression::Integer(val) => Some(Object::Integer(val.value)),
        Expression::Boolean(val) => Some(native_bool_to_bool_object(val.value)),
        Expression::String(val) => Some(Object::String(val.value.clone())),
        Expression::Identifier(val) => Some(eval_identifier(&val.value, env)),
        Expression::PrefixExpression(pe) => {
            let right = match eval_expression(&pe.right, env) {
                Some(val) => val,
                None => return None,
            };
            if let Object::Error(_) = right {
                return Some(right);
            }
            Some(eval_prefix_expression(&pe, &right))
        }
        Expression::InfixExpression(ie) => {
            let left = match eval_expression(&ie.left, env) {
                Some(val) => val,
                None => return None,
            };
            if let Object::Error(_) = left {
                return Some(left);
            }
            let right = match eval_expression(&ie.right, env) {
                Some(val) => val,
                None => return None,
            };
            if let Object::Error(_) = right {
                return Some(right);
            }
            Some(eval_infix_expression(&left, &right, &ie.operator))
        }
        Expression::IfExpression(ife) => eval_if_expression(&ife, env),
        Expression::FunctionLiteral(func) => Some(Object::Function(Function {
            parameters: func.parameters.clone(),
            body: func.body.clone(),
            env: env.clone(),
        })),
        Expression::CallExpression(call) => {
            let func_opt = eval_expression(&call.function, env);
            match func_opt {
                Some(func_obj) => {
                    if func_obj.type_val() == ObjectType::Error {
                        return Some(func_obj);
                    }
                    let args = eval_expressions(&call.arguments, env);
                    if args.len() == 1 && args[0].type_val() == ObjectType::Error {
                        return Some(args[0].clone());
                    }
                    apply_function(&func_obj, &args)
                }
                None => return None,
            }
        }
        Expression::Array(arr) => {
            let elements = eval_expressions(&arr.elements, env);
            if elements.len() == 1 && elements[0].type_val() == ObjectType::Error {
                return Some(elements[0].clone());
            }
            Some(Object::Array(Array { elements }))
        }
        Expression::IndexExpression(idx) => {
            let left = match eval_expression(&idx.left, env) {
                Some(l) => l,
                None => return None,
            };
            if left.type_val() == ObjectType::Error {
                return Some(left);
            }
            let index = match eval_expression(&idx.index, env) {
                Some(l) => l,
                None => return None,
            };
            if index.type_val() == ObjectType::Error {
                return Some(index);
            }
            Some(eval_index_expression(&left, &index))
        }
        Expression::Hash(hash) => eval_hash_literal(hash, env),
    }
}

fn eval_prefix_expression(pe: &PrefixExpression, right: &Object) -> Object {
    match pe.operator {
        PrefixOperator::Bang => eval_bang_operator(right),
        PrefixOperator::Minus => eval_minus_operator(right),
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
        _ => Object::Error(format!("unknown operator: -{}", right.type_string())),
    }
}

fn eval_infix_expression(left: &Object, right: &Object, operator: &InfixOperator) -> Object {
    let lval: i64;
    let rval: i64;
    if left.type_val() != right.type_val() {
        return Object::Error(format!(
            "type mismatch: {} {} {}",
            left.type_string(),
            operator.to_string(),
            right.type_string()
        ));
    }
    match operator {
        InfixOperator::Eq => return native_bool_to_bool_object(left == right),
        InfixOperator::NotEq => return native_bool_to_bool_object(left != right),
        _ => {}
    };
    if left.type_val() == ObjectType::String && right.type_val() == ObjectType::String {
        let lval = match left {
            Object::String(s) => s,
            _ => unreachable!("lval should be a string"),
        };
        let rval = match right {
            Object::String(s) => s,
            _ => unreachable!("lval should be a string"),
        };
        return eval_string_infix_expression(lval, rval, &operator);
    }
    match left {
        Object::Integer(val) => lval = *val,
        _ => {
            return Object::Error(format!(
                "unknown operator: {} {} {}",
                left.type_string(),
                operator.to_string(),
                right.type_string()
            ))
        }
    };
    match right {
        Object::Integer(val) => rval = *val,
        _ => {
            return Object::Error(format!(
                "unknown operator: {} {} {}",
                left.type_string(),
                operator.to_string(),
                right.type_string()
            ))
        }
    };
    eval_integer_infix_expression(lval, rval, operator)
}

fn eval_integer_infix_expression(lval: i64, rval: i64, operator: &InfixOperator) -> Object {
    match operator {
        InfixOperator::Plus => Object::Integer(lval + rval),
        InfixOperator::Minus => Object::Integer(lval - rval),
        InfixOperator::Asterisk => Object::Integer(lval * rval),
        InfixOperator::Slash => Object::Integer(lval / rval),
        InfixOperator::Eq => native_bool_to_bool_object(lval == rval),
        InfixOperator::NotEq => native_bool_to_bool_object(lval != rval),
        InfixOperator::Lt => native_bool_to_bool_object(lval < rval),
        InfixOperator::Gt => native_bool_to_bool_object(lval > rval),
    }
}

fn eval_string_infix_expression(
    lval: &std::sync::Arc<str>,
    rval: &std::sync::Arc<str>,
    operator: &InfixOperator,
) -> Object {
    if *operator != InfixOperator::Plus {
        return Object::Error(format!(
            "unknown operator: {} {} {}",
            "STRING",
            operator.to_string(),
            "STRING",
        ));
    }
    let val = lval.to_string() + &rval.to_string();
    Object::String(val.into())
}

fn eval_if_expression(ife: &IfExpression, env: &mut Environment) -> Option<Object> {
    let cond = match eval_expression(&ife.condition, env) {
        Some(v) => v,
        None => return None,
    };
    if is_truthy(&cond) {
        return eval_block_statments(&ife.consequence.statements, env);
    } else {
        match &ife.alternative {
            Some(alt) => {
                return eval_block_statments(&alt.statements, env);
            }
            None => return Some(NULL),
        }
    }
}

fn eval_block_statments(statements: &Vec<Statement>, env: &mut Environment) -> Option<Object> {
    let mut obj: Option<Object> = None;
    for stmt in statements {
        obj = eval_statement(stmt, env);
        if let Some(o) = obj.clone() {
            match o {
                Object::Return(_) => return Some(o),
                Object::Error(_) => return Some(o),
                _ => {}
            }
        }
    }
    obj
}

fn eval_identifier(name: &std::sync::Arc<str>, env: &Environment) -> Object {
    match env.get(name) {
        Some(v) => v.clone(),
        None => {
            let s = name.to_string();
            if s == "len".to_owned() {
                return LEN;
            }
            if s == "first".to_owned() {
                return FIRST;
            }
            if s == "last".to_owned() {
                return LAST;
            }
            if s == "rest".to_owned() {
                return REST;
            }
            if s == "push".to_owned() {
                return PUSH;
            }
            Object::Error(format!("identifier not found: {}", name))
        }
    }
}

fn eval_expressions(exps: &Vec<Expression>, env: &mut Environment) -> Vec<Object> {
    let mut res = Vec::new();
    for exp in exps.iter() {
        let obj = match eval_expression(&exp, env) {
            Some(o) => o,
            None => return Vec::new(),
        };
        if obj.type_val() == ObjectType::Error {
            res = Vec::new();
            res.push(obj);
            return res;
        }
        res.push(obj);
    }
    res
}

fn apply_function(func_obj: &Object, args: &Vec<Object>) -> Option<Object> {
    match func_obj {
        Object::Function(func) => {
            let mut extended = extend_function_env(func, args);
            let evaluated = eval_block_statments(&func.body.statements, &mut extended);
            match evaluated {
                Some(e) => Some(unwrap_return_value(e)),
                None => None,
            }
        }
        Object::Builtin(builtin) => {
            let fun = builtin.func;
            let r = fun(args);
            Some(r)
        }
        _ => Some(Object::Error(format!(
            "not a function: {}",
            func_obj.type_string()
        ))),
    }
}

fn eval_index_expression(left: &Object, index: &Object) -> Object {
    if left.type_val() == ObjectType::Array && index.type_val() == ObjectType::Integer {
        return eval_array_index_expression(left, index);
    }
    Object::Error(format!(
        "index operator not supported: {}",
        left.type_string()
    ))
}

fn eval_array_index_expression(left: &Object, index: &Object) -> Object {
    let arr = match left {
        Object::Array(a) => a,
        _ => unreachable!("not an array left in eval_array_index_expression"),
    };
    let idx = match index {
        Object::Integer(i) => i,
        _ => unreachable!("not an integer index in eval_array_index_expression"),
    };

    if *idx < 0 || *idx as usize >= arr.elements.len() {
        return NULL;
    }

    return arr.elements[*idx as usize].clone();
}

fn eval_hash_literal(hash: &HashLiteral, env: &mut Environment) -> Option<Object> {
    let mut pairs = Vec::new();
    for pair in hash.pairs.iter() {
        let key = match eval_expression(&pair.0, env) {
            Some(v) => v,
            None => return None,
        };
        if key.type_val() == ObjectType::Error {
            return Some(key);
        }
        let val = match eval_expression(&pair.1, env) {
            Some(v) => v,
            None => return None,
        };
        if val.type_val() == ObjectType::Error {
            return Some(val);
        }
        pairs.push((key, val));
    }
    Some(Object::Hash(Hash { pairs }))
}

fn extend_function_env(func: &Function, args: &Vec<Object>) -> Environment {
    let mut env = Environment::new_enclosed_env(&func.env);

    for (i, param) in func.parameters.iter().enumerate() {
        let arg = args[i].clone();
        env.set(param.value.clone(), arg);
    }

    env
}

fn native_bool_to_bool_object(input: bool) -> Object {
    if input {
        TRUE
    } else {
        FALSE
    }
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(v) => *v,
        _ => true,
    }
}

fn unwrap_return_value(obj: Object) -> Object {
    match obj {
        Object::Return(val) => val.deref().clone(),
        _ => obj,
    }
}

#[cfg(test)]
mod test {
    use crate::{
        ast::Node, environment::Environment, evaluator::eval, lexer::Lexer, object::Object,
        parser::Parser,
    };

    struct IntTest {
        input: &'static str,
        exp: i64,
    }

    struct BoolTest {
        input: &'static str,
        exp: bool,
    }

    struct IfElseTest {
        input: &'static str,
        exp: Option<i64>,
    }

    struct ErrorTest {
        input: &'static str,
        exp: &'static str,
    }

    struct IndexTest {
        input: &'static str,
        exp: Option<i64>,
    }

    fn test_eval(input: &str) -> Option<Object> {
        let mut env = Environment::new();
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse();
        eval(&program, &mut env)
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

    fn test_null_object(obj: &Object) {
        assert_eq!(*obj, Object::Null);
    }

    fn test_string_object(obj: &Object, exp: &str) {
        if let Object::String(s) = obj {
            let full = s.to_string();
            assert_eq!(full, exp.to_string());
        } else {
            panic!("{:#?} is not a string object", obj);
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
            IntTest {
                input: "5 + 5 + 5 + 5 - 10",
                exp: 10,
            },
            IntTest {
                input: "2 * 2 * 2 * 2 * 2",
                exp: 32,
            },
            IntTest {
                input: "-50 + 100 + -50",
                exp: 0,
            },
            IntTest {
                input: "5 * 2 + 10",
                exp: 20,
            },
            IntTest {
                input: "5 + 2 * 10",
                exp: 25,
            },
            IntTest {
                input: "20 + 2 * -10",
                exp: 0,
            },
            IntTest {
                input: "50 / 2 * 2 + 10",
                exp: 60,
            },
            IntTest {
                input: "2 * (5 + 10)",
                exp: 30,
            },
            IntTest {
                input: "3 * 3 * 3 + 10",
                exp: 37,
            },
            IntTest {
                input: "3 * (3 * 3) + 10",
                exp: 37,
            },
            IntTest {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
                exp: 50,
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
            BoolTest {
                input: "1 < 2",
                exp: true,
            },
            BoolTest {
                input: "1 > 2",
                exp: false,
            },
            BoolTest {
                input: "1 < 1",
                exp: false,
            },
            BoolTest {
                input: "1 > 1",
                exp: false,
            },
            BoolTest {
                input: "1 == 1",
                exp: true,
            },
            BoolTest {
                input: "1 != 1",
                exp: false,
            },
            BoolTest {
                input: "1 == 2",
                exp: false,
            },
            BoolTest {
                input: "1 != 2",
                exp: true,
            },
            BoolTest {
                input: "true == true",
                exp: true,
            },
            BoolTest {
                input: "false == false",
                exp: true,
            },
            BoolTest {
                input: "true == false",
                exp: false,
            },
            BoolTest {
                input: "true != false",
                exp: true,
            },
            BoolTest {
                input: "false != true",
                exp: true,
            },
            BoolTest {
                input: "(1 < 2) == true",
                exp: true,
            },
            BoolTest {
                input: "(1 < 2) == false",
                exp: false,
            },
            BoolTest {
                input: "(1 > 2) == true",
                exp: false,
            },
            BoolTest {
                input: "(1 > 2) == false",
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

    #[test]
    fn test_if_else_expressions() {
        let tests = vec![
            IfElseTest {
                input: "if (true) { 10 }",
                exp: Some(10),
            },
            IfElseTest {
                input: "if (false) { 10 }",
                exp: None,
            },
            IfElseTest {
                input: "if (1) { 10 }",
                exp: Some(10),
            },
            IfElseTest {
                input: "if (1 < 2) { 10 }",
                exp: Some(10),
            },
            IfElseTest {
                input: "if (1 > 2) { 10 }",
                exp: None,
            },
            IfElseTest {
                input: "if (1 > 2) { 10 } else { 20 }",
                exp: Some(20),
            },
            IfElseTest {
                input: "if (1 < 2) { 10 } else { 20 }",
                exp: Some(10),
            },
        ];

        for test in tests.iter() {
            let obj_opt = test_eval(test.input);
            if let Some(obj) = obj_opt {
                if let Some(tval) = test.exp {
                    test_int_object(&obj, tval);
                } else {
                    test_null_object(&obj);
                }
            } else {
                panic!("eval returned None");
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            IntTest {
                input: "return 10;",
                exp: 10,
            },
            IntTest {
                input: "return 10; 9;",
                exp: 10,
            },
            IntTest {
                input: "return 2 * 5; 9;",
                exp: 10,
            },
            IntTest {
                input: "9; return 2 * 5; 9;",
                exp: 10,
            },
            IntTest {
                input: "
                    if (10 > 1) {
                        if (10 > 1) {
                            return 10;
                        }
                        return 1;
                    }",
                exp: 10,
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
    fn test_error_handling() {
        let tests = vec![
            ErrorTest {
                input: "5 + true",
                exp: "type mismatch: INTEGER + BOOLEAN",
            },
            ErrorTest {
                input: "5 + true; 5;",
                exp: "type mismatch: INTEGER + BOOLEAN",
            },
            ErrorTest {
                input: "-true",
                exp: "unknown operator: -BOOLEAN",
            },
            ErrorTest {
                input: "true + false;",
                exp: "unknown operator: BOOLEAN + BOOLEAN",
            },
            ErrorTest {
                input: "5; true + false; 5",
                exp: "unknown operator: BOOLEAN + BOOLEAN",
            },
            ErrorTest {
                input: "if (10 > 1) { true + false; }",
                exp: "unknown operator: BOOLEAN + BOOLEAN",
            },
            ErrorTest {
                input: "if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    }
                    return 1;
                }",
                exp: "unknown operator: BOOLEAN + BOOLEAN",
            },
            ErrorTest {
                input: "foobar",
                exp: "identifier not found: foobar",
            },
            ErrorTest {
                input: "\"Hello\" - \"World\"",
                exp: "unknown operator: STRING - STRING",
            },
            ErrorTest {
                input: "len(1)",
                exp: "argument to `len` not supported, got INTEGER",
            },
            ErrorTest {
                input: "len(\"one\", \"two\")",
                exp: "wrong number of arguments. got=2, want=1",
            },
        ];

        for test in tests.iter() {
            let obj = test_eval(test.input);
            match obj {
                Some(v) => match v {
                    Object::Error(v) => assert_eq!(v, test.exp.to_owned()),
                    _ => panic!("{:#?} is not an error object", v),
                },
                None => panic!("eval returned none"),
            }
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = vec![
            IntTest {
                input: "let a = 5; a;",
                exp: 5,
            },
            IntTest {
                input: "let a = 5 * 5; a;",
                exp: 25,
            },
            IntTest {
                input: "let a = 5; let b = a; b;",
                exp: 5,
            },
            IntTest {
                input: "let a = 5; let b = a; let c = a + b + 5; c;",
                exp: 15,
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
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let obj = test_eval(input);
        match obj {
            Some(o) => {
                if let Object::Function(func) = o {
                    assert_eq!(func.parameters.len(), 1);
                    let param1 = &func.parameters[0];
                    assert_eq!(param1.value.to_string(), "x".to_owned());
                    let exp_body = "(x + 2)";
                    let s = func.body.string();
                    assert_eq!(exp_body.to_owned(), s);
                } else {
                    panic!("{:#?} is not a function", o);
                }
            }
            None => panic!("eval returned none"),
        }
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            IntTest {
                input: "let identity = fn(x) { x; }; identity(5);",
                exp: 5,
            },
            IntTest {
                input: "let identity = fn(x) { return x; }; identity(5);",
                exp: 5,
            },
            IntTest {
                input: "let double = fn(x) { x * 2; }; double(5);",
                exp: 10,
            },
            IntTest {
                input: "let add = fn(x, y) { x + y; }; add(5, 5);",
                exp: 10,
            },
            IntTest {
                input: "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                exp: 20,
            },
            IntTest {
                input: "fn(x) { x; }(5)",
                exp: 5,
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
    fn test_closures() {
        let input = "
        let newAdder = fn(x) {
            fn(y) { x + y };
        };
        let addTwo = newAdder(2);
        addTwo(2);";
        let obj_opt = test_eval(input);
        if let Some(obj) = obj_opt {
            test_int_object(&obj, 4);
        } else {
            panic!("evaluator returned None");
        }
    }

    #[test]
    fn test_strings() {
        let input = "\"Hello World!\"";
        let obj_opt = test_eval(input);
        if let Some(obj) = obj_opt {
            if let Object::String(s) = obj {
                assert_eq!(s.to_string(), "Hello World!");
            } else {
                panic!("{:#?} is not a string", obj);
            }
        } else {
            panic!("evaluator returned None");
        }
    }

    #[test]
    fn test_string_concatination() {
        let input = "\"Hello\" + \" \" + \"World!\"";
        let obj_opt = test_eval(input);
        if let Some(obj) = obj_opt {
            if let Object::String(s) = obj {
                assert_eq!(s.to_string(), "Hello World!");
            } else {
                panic!("{:#?} is not a string", obj);
            }
        } else {
            panic!("evaluator returned None");
        }
    }

    #[test]
    fn test_builtin_functions() {
        let tests = vec![
            IntTest {
                input: "len(\"\")",
                exp: 0,
            },
            IntTest {
                input: "len(\"four\")",
                exp: 4,
            },
            IntTest {
                input: "len(\"hello world\")",
                exp: 11,
            },
        ];

        for test in tests {
            let obj_opt = test_eval(test.input);
            if let Some(obj) = obj_opt {
                test_int_object(&obj, test.exp);
            } else {
                panic!("evaluator returned None");
            }
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let obj_opt = test_eval(input);
        if let Some(obj) = obj_opt {
            if let Object::Array(a) = obj {
                test_int_object(&a.elements[0], 1);
                test_int_object(&a.elements[1], 4);
                test_int_object(&a.elements[2], 6);
            } else {
                panic!("{:#?} is not an array obj", obj);
            }
        } else {
            panic!("evaluator returned None");
        }
    }

    #[test]
    fn test_array_index_expression() {
        let tests = vec![
            IndexTest {
                input: "[1, 2, 3][0]",
                exp: Some(1),
            },
            IndexTest {
                input: "[1, 2, 3][1]",
                exp: Some(2),
            },
            IndexTest {
                input: "[1, 2, 3][2]",
                exp: Some(3),
            },
            IndexTest {
                input: "let i = 0; [1][i];",
                exp: Some(1),
            },
            IndexTest {
                input: "[1, 2, 3][1 + 1];",
                exp: Some(3),
            },
            IndexTest {
                input: "let myArray = [1, 2, 3]; myArray[2];",
                exp: Some(3),
            },
            IndexTest {
                input: "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                exp: Some(6),
            },
            IndexTest {
                input: "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                exp: Some(2),
            },
            IndexTest {
                input: "[1, 2, 3][3]",
                exp: None,
            },
            IndexTest {
                input: "[1, 2, 3][-1]",
                exp: None,
            },
        ];

        for test in tests.iter() {
            let obj_opt = test_eval(test.input);
            if let Some(obj) = obj_opt {
                if let Some(tval) = test.exp {
                    test_int_object(&obj, tval);
                } else {
                    test_null_object(&obj);
                }
            } else {
                panic!("eval returned None");
            }
        }
    }

    #[test]
    fn test_hash_literals() {
        let input = "
        let two = \"two\";
        {
            \"one\": 10 - 9,
            two: 1 + 1,
            \"thr\" + \"ee\": 6 / 2,
            4: 4,
            true: 5,
            false: 6
        }
        ";
        let obj_opt = test_eval(input);
        if let Some(obj) = obj_opt {
            if let Object::Hash(hash) = obj {
                assert_eq!(hash.pairs.len(), 6);
                let p1 = &hash.pairs[0];
                test_string_object(&p1.0, "one");
                test_int_object(&p1.1, 1);
                let p2 = &hash.pairs[1];
                test_string_object(&p2.0, "two");
                test_int_object(&p2.1, 2);
                let p3 = &hash.pairs[2];
                test_string_object(&p3.0, "three");
                test_int_object(&p3.1, 3);
                let p4 = &hash.pairs[3];
                test_int_object(&p4.0, 4);
                test_int_object(&p4.1, 4);
                let p5 = &hash.pairs[4];
                test_bool_object(&p5.0, true);
                test_int_object(&p5.1, 5);
                let p6 = &hash.pairs[5];
                test_bool_object(&p6.0, false);
                test_int_object(&p6.1, 6);
            } else {
                panic!("{:#?} is not a hash", obj);
            }
        } else {
            panic!("eval returned None");
        }
    }
}
