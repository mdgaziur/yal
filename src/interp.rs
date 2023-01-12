use crate::ast::{
    BinaryOperation, Expr, ExprContainer, FunStmt, GetExpr, LogicalOperation, Mutable, SetExpr,
    Stmt, StmtContainer, UnaryOperation, VariableExpr,
};
use crate::diagnostics::{Diagnostic, ErrorCode, Severity, Span};
use crate::interner::{InternedString, INTERNER};
use crate::lexer::NumberKind;
use crate::memory::{Allocator, Callable, DataObject, Value};
use crate::native::{
    ArrayPopFunction, FloatFunction, InputFunction, IntFunction, PowFunction, PrintFunction,
    PrintlnFunction, SleepFunction,
};
use crate::session::Session;
use std::cell::RefCell;
use std::collections::HashMap;
use std::str::FromStr;
use std::sync::Arc;

use parking_lot::RwLock;

pub struct Interpreter<'i> {
    allocator: Allocator,
    session: RefCell<&'i mut Session>,
    envs: RefCell<HashMap<InternedString, Vec<Scope>>>,
    current: InternedString,
    context: Vec<InterpreterContext>,
}

impl<'i> Interpreter<'i> {
    pub fn new(session: &'i mut Session) -> Self {
        INTERNER.write().intern_string("main".to_string());
        Self {
            allocator: Allocator::new(),
            session: RefCell::new(session),
            envs: RefCell::new(HashMap::new()),
            current: InternedString::default(),
            context: vec![InterpreterContext::Function], // execution starts at `main` function
        }
    }

    pub fn init(&mut self) -> bool {
        self.session.borrow_mut().parse_entrypoint();
        let session = self.session.borrow();
        if session.has_diagnostics() {
            session.print_diagnostics();
            return false;
        }
        self.current = session.entry_point;
        let ast = session.get_ast(self.current);
        if let Err(e) =
            Self::prepare_global_env_for_module(self.current, ast, &self.envs, &mut self.allocator)
        {
            eprintln!(
                "{}",
                e.display(
                    INTERNER.read().get_interned_string(self.current),
                    &session.files[&self.current]
                )
            );
            return false;
        };
        if session.debug {
            dbg!(&self.envs);
            dbg!(ast);
        }

        true
    }

    pub fn interpret(&mut self) {
        let session = self.session.borrow();
        // FIXME: remove clone
        let ast = session.get_ast(self.current).to_vec();
        drop(session);
        for stmt in &ast {
            if let Err(e) = self.interpret_stmt(stmt) {
                eprintln!(
                    "{}",
                    e.display(
                        INTERNER.read().get_interned_string(self.current),
                        &self.session.borrow().files[&self.current]
                    )
                );
                return;
            }
        }
        if let Err(e) = self.interpret_main() {
            eprintln!(
                "{}",
                e.display(
                    INTERNER.read().get_interned_string(self.current),
                    &self.session.borrow().files[&self.current]
                )
            );
        }
    }

    pub fn interpret_main(&mut self) -> Result<(), Diagnostic> {
        let envs = self.envs.borrow();
        let Some(main_function_env_item) = envs
            .get(&self.current)
            .unwrap()
            .last()
            .unwrap()
            .get(&INTERNER.read().get_intern_addr_for_string("main").unwrap()) else {
            return Err(Diagnostic {
                severity: Severity::Error,
                code: ErrorCode::NoMain,
                message: "No `main` function in module".to_string(),
                span: Span { lo: 0, hi: usize::MAX }
            })
        };
        let main_function_container = main_function_env_item.clone().value;
        let main_function_read = main_function_container.read();
        drop(envs);
        match &*main_function_read {
            Value::Function(fun) => {
                fun.call(self, vec![])?;
                Ok(())
            }
            _ => Err(Diagnostic {
                severity: Severity::Error,
                code: ErrorCode::NoMain,
                message: "`main` must be a function".to_string(),
                span: Span {
                    lo: 0,
                    hi: usize::MAX,
                },
            }),
        }
    }

    fn interpret_stmt(&mut self, stmt: &StmtContainer) -> Result<(), Diagnostic> {
        match &stmt.stmt {
            Stmt::Var(var_stmt) => {
                let interpreted_value = self.interpret_expr(&var_stmt.value)?;
                let _ = &mut self
                    .envs
                    .borrow_mut()
                    .get_mut(&self.current)
                    .unwrap()
                    .last_mut()
                    .unwrap()
                    .insert(
                        var_stmt.name,
                        EnvItem {
                            mutable: var_stmt.mutable,
                            value: interpreted_value,
                        },
                    );
            }
            Stmt::Loop(block) => loop {
                if let Err(e) = self.interpret_stmt(block) {
                    if matches!(e.code, ErrorCode::Break) {
                        return Ok(());
                    } else if !matches!(e.code, ErrorCode::Continue) {
                        return Err(e);
                    }
                }
            },
            Stmt::Block(stmts) => {
                let scope = Scope::new();
                self.envs
                    .borrow_mut()
                    .get_mut(&self.current)
                    .unwrap()
                    .push(scope);
                for stmt in stmts {
                    if let Err(e) = self.interpret_stmt(stmt) {
                        self.envs.borrow_mut().get_mut(&self.current).unwrap().pop();
                        self.allocator.dealloc_unused_objects();
                        return Err(e);
                    }
                }

                self.envs.borrow_mut().get_mut(&self.current).unwrap().pop();
                self.allocator.dealloc_unused_objects();
            }
            Stmt::If(if_stmt) => {
                let condition_container = self.interpret_expr(&if_stmt.condition)?;
                let condition_read = condition_container.read();
                let is_condition_met =
                    !matches!(&*condition_read, Value::None | Value::Boolean(false));

                if is_condition_met {
                    self.interpret_stmt(&if_stmt.block)?;
                } else if let Some(else_) = &if_stmt.else_ {
                    self.interpret_stmt(else_)?;
                } else if let Some(then) = &if_stmt.then {
                    self.interpret_stmt(then)?;
                }

                return Ok(());
            }
            Stmt::Iter(iter_stmt) => {
                let iterable_container = self.interpret_expr(&iter_stmt.iterable)?;
                let iterable_read = iterable_container.read();
                let array = match &*iterable_read {
                    Value::Array(arr) => arr,
                    val => {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            code: ErrorCode::InvalidType,
                            message: format!("Cannot iter type `{}`", val.type_()),
                            span: iter_stmt.iterable.span,
                        })
                    }
                };

                for item in array {
                    let mut scope = Scope::new();
                    scope.insert(
                        iter_stmt.binding,
                        EnvItem {
                            mutable: Mutable::No,
                            value: item.clone(),
                        },
                    );
                    self.envs
                        .borrow_mut()
                        .get_mut(&self.current)
                        .unwrap()
                        .push(scope);

                    if let Err(e) = self.interpret_stmt(&iter_stmt.block) {
                        if matches!(e.code, ErrorCode::Break) {
                            return Ok(());
                        } else if !matches!(e.code, ErrorCode::Continue) {
                            return Err(e);
                        }
                    }

                    self.envs.borrow_mut().get_mut(&self.current).unwrap().pop();
                }
            }
            Stmt::Ret(v) => {
                let val = self.interpret_expr(v)?;

                return Err(Diagnostic {
                    severity: Severity::Error,
                    code: ErrorCode::Return(val),
                    message: String::from("This error should never be displayed"),
                    span: stmt.span,
                });
            }
            Stmt::Break => {
                return Err(Diagnostic {
                    severity: Severity::Warning,
                    span: stmt.span,
                    message: String::new(),
                    code: ErrorCode::Break,
                })
            }
            Stmt::Continue => {
                return Err(Diagnostic {
                    severity: Severity::Warning,
                    span: stmt.span,
                    message: String::new(),
                    code: ErrorCode::Continue,
                })
            }
            Stmt::Expr(expr) => {
                self.interpret_expr(expr)?;
            }
            Stmt::Data(_) => (),
            Stmt::Methods(_) => (),
            Stmt::Fun(_) => (),
        }

        Ok(())
    }

    fn get_mutability(&self, name: InternedString) -> Option<Mutable> {
        let envs = self.envs.borrow();
        let scopes = envs[&self.current].iter().rev();

        for scope in scopes {
            if let Some(env_item) = scope.get(&name) {
                return Some(env_item.mutable);
            }
        }

        None
    }

    fn interpret_expr(&mut self, expr: &ExprContainer) -> Result<Arc<RwLock<Value>>, Diagnostic> {
        let val = match &expr.expr {
            Expr::Assignment(assignment_expr) => {
                let v_expr = match &assignment_expr.lvalue.expr {
                    Expr::Variable(v_expr) => v_expr,
                    _ => unreachable!(),
                };
                let name = v_expr.name;
                if !v_expr.data_member {
                    if let Some(mutability) = self.get_mutability(name) {
                        if mutability == Mutable::No {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                code: ErrorCode::MutabilityError,
                                message: String::from("Attempt to mutate immutable variable"),
                                span: expr.span,
                            });
                        }
                    } else {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            code: ErrorCode::UndefinedVariable,
                            message: String::from("Undefined variable/type"),
                            span: expr.span,
                        });
                    }
                } else {
                    return self.interpret_expr(
                        &Expr::Set(Box::new(SetExpr {
                            property: name,
                            value: assignment_expr.rvalue.clone(),
                            object: Expr::Variable(VariableExpr {
                                name: INTERNER.read().get_intern_addr_for_string("@").unwrap(),
                                data_member: false,
                            })
                            .into_container(expr.span),
                        }))
                        .into_container(expr.span),
                    );
                }

                let rvalue = self.interpret_expr(&assignment_expr.rvalue)?;
                let mut envs = self.envs.borrow_mut();
                let scopes = envs.get_mut(&self.current).unwrap().iter_mut().rev();

                for scope in scopes {
                    if scope.get_mut(&name).is_some() {
                        scope.insert(
                            name,
                            EnvItem {
                                mutable: Mutable::Yes,
                                value: rvalue.clone(),
                            },
                        );
                        break;
                    }
                }

                return Ok(rvalue);
            }
            Expr::Data(data_expr) => {
                let data_container = self.interpret_expr(&ExprContainer {
                    span: expr.span,
                    expr: Expr::Variable(VariableExpr {
                        name: data_expr.name,
                        data_member: false,
                    }),
                })?;
                let data_read = data_container.read();
                let (data_stmt, data_methods) = match &*data_read {
                    Value::Data(ds, dm) => (ds, dm.clone()),
                    val => {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            code: ErrorCode::InvalidType,
                            message: format!("Expected data, found type `{}`", val.type_()),
                            span: expr.span,
                        })
                    }
                };

                for prop in &data_stmt.fields {
                    if !data_expr.props.contains_key(prop) {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            code: ErrorCode::MissingProp,
                            message: format!(
                                "Missing property `{}`",
                                INTERNER.read().get_interned_string(*prop)
                            ),
                            span: expr.span,
                        });
                    }
                }

                for prop in data_expr.props.keys() {
                    if !data_stmt.fields.contains(prop) {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            code: ErrorCode::UnknownProp,
                            message: format!(
                                "Unknown property `{}`",
                                INTERNER.read().get_interned_string(*prop)
                            ),
                            span: expr.span,
                        });
                    }
                }

                let mut values = HashMap::new();
                for (prop, value) in &data_expr.props {
                    let value = self.interpret_expr(value)?;
                    values.insert(*prop, value);
                }

                Ok(Value::DataObject(DataObject {
                    name: data_expr.name,
                    values,
                    methods: data_methods,
                }))
            }
            Expr::Boolean(b) => Ok(Value::Boolean(*b)),
            Expr::Number(ne) => {
                let radix = ne.kind.radix();
                let number = INTERNER.read().get_interned_string(ne.value).to_string();

                match ne.kind {
                    NumberKind::DecFloat => match f64::from_str(&number) {
                        Ok(v) => Ok(Value::Float(v)),
                        Err(e) => Err(Diagnostic {
                            span: expr.span,
                            message: format!("Failed to parse number: `{}`", e),
                            code: ErrorCode::InvalidNumber,
                            severity: Severity::Error,
                        }),
                    },
                    _ => match i64::from_str_radix(&number, radix) {
                        Ok(v) => Ok(Value::Int(v)),
                        Err(e) => Err(Diagnostic {
                            span: expr.span,
                            message: format!("Failed to parse number: `{}`", e),
                            code: ErrorCode::InvalidNumber,
                            severity: Severity::Error,
                        }),
                    },
                }
            }
            Expr::Logical(logical_expr) => {
                let lhs_container = self.interpret_expr(&logical_expr.lhs)?;
                let lhs_inner = lhs_container.read();
                let rhs_container = self.interpret_expr(&logical_expr.rhs)?;
                let rhs_inner = rhs_container.read();
                let lhs_type = lhs_inner.type_();
                let rhs_type = rhs_inner.type_();
                let op = logical_expr.op;
                let return_error = move || {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        code: ErrorCode::InvalidType,
                        message: format!(
                            "Cannot apply logical operation between `{}` and `{}`",
                            lhs_type, rhs_type
                        ),
                        span: expr.span,
                    });
                };

                let res = match (&*lhs_inner, &*rhs_inner) {
                    (Value::String(lhs), Value::String(rhs)) => {
                        let res;

                        {
                            let interner = INTERNER.read();
                            let lhs_s = interner.get_interned_string(*lhs);
                            let rhs_s = interner.get_interned_string(*rhs);
                            res = Value::Boolean({
                                match op {
                                    LogicalOperation::GreaterThan => lhs_s.len() > rhs_s.len(),
                                    LogicalOperation::LessThan => lhs_s.len() < rhs_s.len(),
                                    LogicalOperation::GreaterThanOrEquals => {
                                        lhs_s.len() >= rhs_s.len()
                                    }
                                    LogicalOperation::LessThanOrEquals => {
                                        lhs_s.len() <= rhs_s.len()
                                    }
                                    LogicalOperation::Equals => lhs_s == rhs_s,
                                    LogicalOperation::NotEquals => lhs_s != rhs_s,
                                    LogicalOperation::And => !lhs_s.is_empty() && !rhs_s.is_empty(),
                                    LogicalOperation::Or => !lhs_s.is_empty() || !rhs_s.is_empty(),
                                }
                            });
                        }

                        res
                    }
                    (Value::Int(lhs), Value::Float(rhs)) => {
                        let lhs = *lhs as f64;
                        let rhs = *rhs;
                        Value::Boolean(match op {
                            LogicalOperation::Equals => lhs == rhs,
                            LogicalOperation::NotEquals => lhs != rhs,
                            LogicalOperation::GreaterThan => lhs > rhs,
                            LogicalOperation::LessThan => lhs < rhs,
                            LogicalOperation::GreaterThanOrEquals => lhs >= rhs,
                            LogicalOperation::LessThanOrEquals => lhs <= rhs,
                            LogicalOperation::And => (lhs != 0.0) && (rhs != 0.0),
                            LogicalOperation::Or => (lhs != 0.0) || (rhs != 0.0),
                        })
                    }
                    (Value::Float(lhs), Value::Int(rhs)) => {
                        let lhs = *lhs;
                        let rhs = *rhs as f64;
                        Value::Boolean(match op {
                            LogicalOperation::Equals => lhs == rhs,
                            LogicalOperation::NotEquals => lhs != rhs,
                            LogicalOperation::GreaterThan => lhs > rhs,
                            LogicalOperation::LessThan => lhs < rhs,
                            LogicalOperation::GreaterThanOrEquals => lhs >= rhs,
                            LogicalOperation::LessThanOrEquals => lhs <= rhs,
                            LogicalOperation::And => (lhs != 0.0) && (rhs != 0.0),
                            LogicalOperation::Or => (lhs != 0.0) || (rhs != 0.0),
                        })
                    }
                    (Value::Int(lhs), Value::Boolean(rhs))
                    | (Value::Boolean(rhs), Value::Int(lhs)) => {
                        let lhs = *lhs;
                        let rhs = *rhs;
                        Value::Boolean(match op {
                            LogicalOperation::And => (lhs != 0) && rhs,
                            LogicalOperation::Or => (lhs != 0) || rhs,
                            _ => return return_error(),
                        })
                    }
                    (Value::Float(lhs), Value::Boolean(rhs))
                    | (Value::Boolean(rhs), Value::Float(lhs)) => {
                        let lhs = *lhs;
                        let rhs = *rhs;
                        Value::Boolean(match op {
                            LogicalOperation::And => (lhs != 0.0) && rhs,
                            LogicalOperation::Or => (lhs != 0.0) || rhs,
                            _ => return return_error(),
                        })
                    }
                    (Value::Boolean(lhs), Value::Boolean(rhs)) => {
                        let lhs = *lhs;
                        let rhs = *rhs;
                        Value::Boolean(match op {
                            LogicalOperation::And => lhs && rhs,
                            LogicalOperation::Or => lhs || rhs,
                            _ => return return_error(),
                        })
                    }
                    (Value::Int(lhs), Value::Int(rhs)) => {
                        let lhs = *lhs;
                        let rhs = *rhs;
                        Value::Boolean(match op {
                            LogicalOperation::Equals => lhs == rhs,
                            LogicalOperation::NotEquals => lhs != rhs,
                            LogicalOperation::GreaterThan => lhs > rhs,
                            LogicalOperation::LessThan => lhs < rhs,
                            LogicalOperation::GreaterThanOrEquals => lhs >= rhs,
                            LogicalOperation::LessThanOrEquals => lhs <= rhs,
                            LogicalOperation::And => (lhs != 0) && (rhs != 0),
                            LogicalOperation::Or => (lhs != 0) || (rhs != 0),
                        })
                    }
                    _ => return return_error(),
                };

                Ok(res)
            }
            Expr::Binary(binary_expr) => {
                let lhs_container = self.interpret_expr(&binary_expr.lhs)?;
                let lhs_inner = lhs_container.read();
                let rhs_container = self.interpret_expr(&binary_expr.rhs)?;
                let rhs_inner = rhs_container.read();
                let lhs_type = lhs_inner.type_();
                let rhs_type = rhs_inner.type_();
                let op = binary_expr.op;
                let return_error = move || {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        code: ErrorCode::InvalidType,
                        message: format!(
                            "Cannot apply binary operation between `{}` and `{}`",
                            lhs_type, rhs_type
                        ),
                        span: expr.span,
                    });
                };

                let res = match (&*lhs_inner, &*rhs_inner) {
                    (Value::Int(lhs), Value::Float(rhs)) => match op {
                        BinaryOperation::Plus => Value::Float(*lhs as f64 + *rhs),
                        BinaryOperation::Minus => Value::Float(*lhs as f64 - *rhs),
                        BinaryOperation::Multiply => Value::Float(*lhs as f64 * *rhs),
                        BinaryOperation::Divide => Value::Float(*lhs as f64 / *rhs),
                        BinaryOperation::Modulus => Value::Float(*lhs as f64 % *rhs),
                        _ => return return_error(),
                    },
                    (Value::Float(lhs), Value::Int(rhs)) => match op {
                        BinaryOperation::Plus => Value::Float(*lhs + *rhs as f64),
                        BinaryOperation::Minus => Value::Float(*lhs - *rhs as f64),
                        BinaryOperation::Multiply => Value::Float(*lhs * *rhs as f64),
                        BinaryOperation::Divide => Value::Float(*lhs / *rhs as f64),
                        BinaryOperation::Modulus => Value::Float(*lhs % *rhs as f64),
                        _ => return return_error(),
                    },
                    (Value::Float(lhs), Value::Float(rhs)) => match op {
                        BinaryOperation::Plus => Value::Float(*lhs + *rhs),
                        BinaryOperation::Minus => Value::Float(*lhs - *rhs),
                        BinaryOperation::Multiply => Value::Float(*lhs * *rhs),
                        BinaryOperation::Divide => Value::Float(*lhs / *rhs),
                        BinaryOperation::Modulus => Value::Float(*lhs % *rhs),
                        _ => return return_error(),
                    },
                    (Value::Int(lhs), Value::Int(rhs)) => match op {
                        BinaryOperation::Plus => Value::Int(*lhs + *rhs),
                        BinaryOperation::Minus => Value::Int(*lhs - *rhs),
                        BinaryOperation::Multiply => Value::Int(*lhs * *rhs),
                        BinaryOperation::Divide => Value::Float(*lhs as f64 / *rhs as f64),
                        BinaryOperation::Modulus => Value::Int(*lhs % *rhs),
                        BinaryOperation::And => Value::Int(*lhs & *rhs),
                        BinaryOperation::Or => Value::Int(*lhs | *rhs),
                        BinaryOperation::Xor => Value::Int(*lhs ^ *rhs),
                        BinaryOperation::LeftShift => Value::Int(*lhs << *rhs),
                        BinaryOperation::RightShift => Value::Int(*lhs >> *rhs),
                    },
                    (Value::String(is), Value::Int(n)) => match op {
                        BinaryOperation::Multiply => Value::String({
                            let res = INTERNER.read().get_interned_string(*is).repeat(*n as usize);

                            INTERNER.write().intern_string(res)
                        }),
                        _ => return return_error(),
                    },
                    (Value::Array(arr), _) => {
                        let mut new_arr = arr.clone();
                        new_arr.push(rhs_container.clone());
                        let arr = self.allocator.allocate(Value::Array(new_arr));

                        return Ok(arr);
                    }
                    _ => return return_error(),
                };

                Ok(res)
            }
            Expr::Unary(unary_expr) => {
                let value_container = self.interpret_expr(&unary_expr.expr)?;
                let value_read = value_container.read();

                match unary_expr.op {
                    UnaryOperation::Not => match &*value_read {
                        Value::None | Value::Boolean(false) => Ok(Value::Boolean(true)),
                        _ => Ok(Value::Boolean(false)),
                    },
                    UnaryOperation::Negate => match &*value_read {
                        Value::Int(i) => Ok(Value::Int(-*i)),
                        Value::Float(f) => Ok(Value::Float(-*f)),
                        v => Err(Diagnostic {
                            severity: Severity::Error,
                            code: ErrorCode::InvalidUnaryOperation,
                            message: format!("Cannot apply unary negate to type `{}`", v.type_()),
                            span: expr.span,
                        }),
                    },
                }
            }
            Expr::Call(call_expr) => {
                let function_container = self.interpret_expr(&call_expr.callee)?;
                let function_read = function_container.read();
                let function = match &*function_read {
                    Value::Function(function) => function,
                    val => {
                        return Err(Diagnostic {
                            span: expr.span,
                            message: format!("Cannot call type: `{}`", val.type_()),
                            code: ErrorCode::InvalidType,
                            severity: Severity::Error,
                        })
                    }
                };
                if function.arity() != usize::MAX && call_expr.args.len() != function.arity() {
                    return Err(Diagnostic {
                        span: expr.span,
                        message: format!(
                            "Function expects {} arguments, found {} arguments",
                            function.arity(),
                            call_expr.args.len()
                        ),
                        code: ErrorCode::ArityError,
                        severity: Severity::Error,
                    });
                }

                let mut args = vec![];
                for expr in &call_expr.args {
                    let val = self.interpret_expr(expr)?;
                    args.push(self.allocator.clone(&val));
                }

                let res = function.call(self, args);
                if let Ok(v) = res {
                    return Ok(v.unwrap_or_else(|| self.allocator.get_none()));
                } else if let Err(mut e) = res {
                    if e.span
                        == (Span {
                            lo: usize::MAX,
                            hi: usize::MAX,
                        })
                    {
                        e.span = expr.span;
                    }

                    return Err(e);
                }

                unreachable!()
            }
            Expr::None => Ok(Value::None),
            Expr::String(is) => Ok(Value::String(*is)),
            Expr::Grouping(e) => return self.interpret_expr(e),
            Expr::Get(get_expr) => {
                let object_container = self.interpret_expr(&get_expr.object)?;
                let object_read = object_container.read();
                let object = match &*object_read {
                    Value::DataObject(dobj) => dobj,
                    _ => {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            code: ErrorCode::InvalidType,
                            message: format!(
                                "Cannot access property from type `{}`",
                                object_read.type_()
                            ),
                            span: expr.span,
                        })
                    }
                };
                if let Some(value) = object.values.get(&get_expr.property) {
                    return Ok(value.clone());
                } else if let Some(method) = object.methods.get(&get_expr.property) {
                    return Ok(self
                        .allocator
                        .allocate(Value::Function(Box::new(YalFunction {
                            fun_stmt: method.clone(),
                            data: Some(object_container.clone()),
                        }))));
                } else {
                    Err(Diagnostic {
                        severity: Severity::Error,
                        code: ErrorCode::InvalidType,
                        message: format!(
                            "Cannot access property `{}`",
                            INTERNER.read().get_interned_string(get_expr.property)
                        ),
                        span: expr.span,
                    })
                }
            }
            Expr::Array(array) => {
                let mut items = vec![];
                for expr in array {
                    let value = self.interpret_expr(expr)?;
                    items.push(value);
                }

                Ok(Value::Array(items))
            }
            Expr::Index(index_expr) => {
                let array_container = self.interpret_expr(&index_expr.object)?;
                let array_read = array_container.read();
                let array = match &*array_read {
                    Value::Array(a) => a,
                    _ => {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            code: ErrorCode::InvalidType,
                            message: format!("Cannot index type `{}`", array_read.type_()),
                            span: expr.span,
                        })
                    }
                };

                let index_container = self.interpret_expr(&index_expr.index)?;
                let index = match &*index_container.read() {
                    Value::Int(idx) => *idx,
                    ty => {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            code: ErrorCode::InvalidType,
                            message: format!("Cannot index using type `{}`", ty.type_()),
                            span: expr.span,
                        })
                    }
                };

                return if let Some(val) = array.get(index as usize) {
                    Ok(val.clone())
                } else {
                    Err(Diagnostic {
                        severity: Severity::Error,
                        code: ErrorCode::InvalidType,
                        message: String::from("Out of bound read"),
                        span: expr.span,
                    })
                };
            }
            Expr::IndexSet(indexset_expr) => {
                let index_expr = match &indexset_expr.object.expr {
                    Expr::Index(index_expr) => index_expr,
                    _ => unreachable!(),
                };
                let array_container = self.interpret_expr(&index_expr.object)?;

                let index_expr = self.interpret_expr(&index_expr.index)?;
                let index = match &*index_expr.read() {
                    Value::Int(idx) => *idx,
                    ty => {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            code: ErrorCode::InvalidType,
                            message: format!("Cannot index using type `{}`", ty.type_()),
                            span: expr.span,
                        })
                    }
                };

                let value = self.interpret_expr(&indexset_expr.value)?;
                let mut array_write = array_container.write();
                let array = match &mut *array_write {
                    Value::Array(a) => a,
                    _ => {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            code: ErrorCode::InvalidType,
                            message: format!("Cannot index type `{}`", array_write.type_()),
                            span: expr.span,
                        })
                    }
                };

                if array.get(index as usize).is_none() {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        code: ErrorCode::InvalidType,
                        message: String::from("Out of bound read"),
                        span: expr.span,
                    });
                };

                array[index as usize] = value.clone();
                return Ok(value);
            }
            Expr::Set(set_expr) => {
                if let Expr::Get(get_expr) = &set_expr.object.expr {
                    let value = self.interpret_expr(&set_expr.value)?;
                    let obj_container = self.interpret_expr(&get_expr.object)?;
                    let mut obj_write = obj_container.write();
                    let data_object = match &mut *obj_write {
                        Value::DataObject(data_object) => data_object,
                        _ => {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                code: ErrorCode::InvalidType,
                                message: format!(
                                    "Cannot access properties of type `{}`",
                                    obj_write.type_()
                                ),
                                span: set_expr.object.span,
                            })
                        }
                    };

                    if let std::collections::hash_map::Entry::Occupied(mut e) =
                        data_object.values.entry(get_expr.property)
                    {
                        e.insert(value.clone());
                    } else {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            code: ErrorCode::InvalidType,
                            message: format!(
                                "Cannot access property `{}`",
                                INTERNER.read().get_interned_string(get_expr.property)
                            ),
                            span: set_expr.object.span,
                        });
                    }

                    return Ok(value);
                }

                let object_container = self.interpret_expr(&set_expr.object)?;
                let value = self.interpret_expr(&set_expr.value)?;
                let object_read = object_container.read();
                let object = match &*object_read {
                    Value::DataObject(dobj) => dobj,
                    _ => {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            code: ErrorCode::InvalidType,
                            message: format!(
                                "Cannot access property from type `{}`",
                                object_read.type_()
                            ),
                            span: expr.span,
                        })
                    }
                };
                if object.values.get(&set_expr.property).is_none() {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        code: ErrorCode::InvalidType,
                        message: format!(
                            "Cannot access property `{}`",
                            INTERNER.read().get_interned_string(set_expr.property)
                        ),
                        span: expr.span,
                    });
                };

                drop(object_read);
                let mut object_write = object_container.write();
                let object = match &mut *object_write {
                    Value::DataObject(dobj) => dobj,
                    _ => {
                        return Err(Diagnostic {
                            severity: Severity::Error,
                            code: ErrorCode::InvalidType,
                            message: format!(
                                "Cannot access property from type `{}`",
                                object_write.type_()
                            ),
                            span: expr.span,
                        })
                    }
                };
                object.values.insert(set_expr.property, value);
                return Ok(object_container.clone());
            }
            Expr::Variable(v_expr) => {
                let envs = self.envs.borrow_mut();
                let scopes = envs[&self.current].iter().rev();

                if v_expr.data_member {
                    drop(envs);
                    if *self.context.last().unwrap() == InterpreterContext::Method {
                        return self.interpret_expr(
                            &Expr::Get(Box::new(GetExpr {
                                object: Expr::Variable(VariableExpr {
                                    name: INTERNER.read().get_intern_addr_for_string("@").unwrap(),
                                    data_member: false,
                                })
                                .into_container(expr.span),
                                property: v_expr.name,
                            }))
                            .into_container(expr.span),
                        );
                    }
                } else {
                    for scope in scopes {
                        if let Some(env_item) = scope.get(&v_expr.name) {
                            return Ok(env_item.value.clone());
                        }
                    }
                }

                Err(Diagnostic {
                    severity: Severity::Error,
                    code: ErrorCode::UndefinedVariable,
                    message: String::from("Undefined variable/type"),
                    span: expr.span,
                })
            }
        };

        Ok(self.allocator.allocate(val?))
    }

    pub fn prepare_global_env_for_module(
        name: InternedString,
        ast: &[StmtContainer],
        interpreter_env: &RefCell<HashMap<InternedString, Vec<Scope>>>,
        allocator: &mut Allocator,
    ) -> Result<(), Diagnostic> {
        let mut env = vec![];
        let mut scope = Scope::new();

        let mut create_native_fn =
            |allocator: &mut Allocator, name: &str, function: Box<dyn Callable>| -> EnvItem {
                let env_item = EnvItem {
                    mutable: Mutable::No,
                    value: allocator.allocate(Value::Function(function)),
                };
                scope.insert(
                    INTERNER.write().intern_string(name.to_string()),
                    env_item.clone(),
                );
                env_item
            };

        create_native_fn(allocator, "print", Box::new(PrintFunction {}));
        create_native_fn(allocator, "println", Box::new(PrintlnFunction {}));
        create_native_fn(allocator, "sleep", Box::new(SleepFunction {}));
        create_native_fn(allocator, "pop", Box::new(ArrayPopFunction {}));
        create_native_fn(allocator, "input", Box::new(InputFunction {}));
        create_native_fn(allocator, "int", Box::new(IntFunction {}));
        create_native_fn(allocator, "float", Box::new(FloatFunction {}));
        create_native_fn(allocator, "pow", Box::new(PowFunction {}));

        for stmt in ast {
            match &stmt.stmt {
                Stmt::Fun(fun_stmt) => {
                    scope.insert(
                        fun_stmt.name,
                        EnvItem {
                            mutable: Mutable::No,
                            value: allocator.allocate(Value::Function(Box::new(YalFunction {
                                fun_stmt: fun_stmt.clone(),
                                data: None,
                            }))),
                        },
                    );
                }
                Stmt::Data(data_stmt) => {
                    let value = allocator.allocate(Value::Data(data_stmt.clone(), HashMap::new()));
                    scope.insert(
                        data_stmt.name,
                        EnvItem {
                            mutable: Mutable::No,
                            value,
                        },
                    );
                }
                Stmt::Methods(methods_stmt) => {
                    let data_container = match scope.get(&methods_stmt.data) {
                        Some(ei) => ei.value.clone(),
                        _ => {
                            return Err(Diagnostic {
                                severity: Severity::Error,
                                code: ErrorCode::InvalidType,
                                span: stmt.span,
                                message: String::from("Cannot bind methods to invalid type"),
                            })
                        }
                    };
                    let mut data = data_container.write();

                    let data_methods = match &mut *data {
                        Value::Data(_, methods) => methods,
                        _ => unreachable!(),
                    };

                    for method in methods_stmt.methods.clone() {
                        match method.stmt {
                            Stmt::Fun(fun) => {
                                data_methods.insert(fun.name, fun);
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                _ => (),
            }
        }

        env.push(scope);
        interpreter_env.borrow_mut().insert(name, env);
        Ok(())
    }

    pub fn get_allocator_mut(&mut self) -> &mut Allocator {
        &mut self.allocator
    }

    #[allow(unused)]
    pub fn get_allocator(&self) -> &Allocator {
        &self.allocator
    }
}

#[derive(Debug, Clone)]
struct YalFunction {
    fun_stmt: Box<FunStmt>,
    data: Option<Arc<RwLock<Value>>>,
}

impl Callable for YalFunction {
    fn arity(&self) -> usize {
        self.fun_stmt.arguments.len()
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Arc<RwLock<Value>>>,
    ) -> Result<Option<Arc<RwLock<Value>>>, Diagnostic> {
        let mut env = interpreter.envs.borrow_mut();
        let mut scope = HashMap::new();
        for (k, v) in self.fun_stmt.arguments.iter().zip(args.iter()) {
            scope.insert(
                *k,
                EnvItem {
                    value: v.clone(),
                    mutable: Mutable::No,
                },
            );
        }
        if let Some(data) = self.data.clone() {
            scope.insert(
                INTERNER.write().intern_string("@".to_string()),
                EnvItem {
                    value: data,
                    mutable: Mutable::Yes,
                },
            );
            interpreter.context.push(InterpreterContext::Method);
        } else {
            interpreter.context.push(InterpreterContext::Function);
        }
        let parent_scope = if env.get_mut(&interpreter.current).unwrap().len() > 1 {
            env.get_mut(&interpreter.current).unwrap().pop()
        } else {
            None
        };
        env.get_mut(&interpreter.current).unwrap().push(scope);
        drop(env);

        match &self.fun_stmt.body.stmt {
            Stmt::Block(stmts) => {
                for stmt in stmts {
                    if let Err(err) = interpreter.interpret_stmt(stmt) {
                        return if let ErrorCode::Return(rv) = err.code {
                            interpreter.context.pop();
                            let mut env = interpreter.envs.borrow_mut();
                            env.get_mut(&interpreter.current).unwrap().pop();
                            interpreter.allocator.dealloc_unused_objects();
                            if let Some(parent_scope) = parent_scope {
                                env.get_mut(&interpreter.current)
                                    .unwrap()
                                    .push(parent_scope);
                            }
                            Ok(Some(rv))
                        } else {
                            Err(err)
                        };
                    }
                }
            }
            _ => unreachable!(),
        }

        interpreter.context.pop();
        let mut env = interpreter.envs.borrow_mut();
        env.get_mut(&interpreter.current).unwrap().pop();
        interpreter.allocator.dealloc_unused_objects();
        if let Some(parent_scope) = parent_scope {
            env.get_mut(&interpreter.current)
                .unwrap()
                .push(parent_scope);
        }
        Ok(None)
    }

    fn to_string(&self) -> String {
        format!(
            "<function `{}` at {:p}>",
            INTERNER.read().get_interned_string(self.fun_stmt.name),
            self as *const _
        )
    }

    fn clone(&self) -> Self
    where
        Self: Sized,
    {
        Clone::clone(self)
    }
}

type Scope = HashMap<InternedString, EnvItem>;

#[derive(Debug, Clone, Copy, PartialEq)]
enum InterpreterContext {
    Function,
    Method,
}

#[derive(Debug, Clone)]
pub struct EnvItem {
    pub mutable: Mutable,
    pub value: Arc<RwLock<Value>>,
}
