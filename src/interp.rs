use crate::ast::{
    BinaryOperation, Expr, ExprContainer, FunStmt, LogicalOperation, Mutable, Stmt, StmtContainer,
    UnaryOperation,
};
use crate::diagnostics::{Diagnostic, ErrorCode, Severity, Span};
use crate::interner::{InternedString, INTERNER};
use crate::lexer::NumberKind;
use crate::memory::{Allocator, Callable, DataObject, Value, ValueAddr};
use crate::native::{ArrayPopFunction, PrintFunction, PrintlnFunction, SleepFunction};
use crate::Session;
use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::str::FromStr;

pub struct Interpreter<'i> {
    allocator: Allocator,
    session: RefCell<&'i mut Session>,
    envs: RefCell<HashMap<InternedString, Vec<Scope>>>,
    current: InternedString,
    context: Vec<InterpreterContext>,
}

impl<'i> Interpreter<'i> {
    pub fn new(session: &'i mut Session) -> Self {
        Self {
            allocator: Allocator::new(),
            session: RefCell::new(session),
            envs: RefCell::new(HashMap::new()),
            current: InternedString::default(),
            context: vec![],
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
        let ast = session.get_ast(self.current).to_vec(); // TODO: don't use a lot of memory :|
        drop(session);
        if let Err(e) = self.interpret_ast(&ast) {
            eprintln!(
                "{}",
                e.display(
                    INTERNER.read().get_interned_string(self.current),
                    &self.session.borrow().files[&self.current]
                )
            );
        }
    }

    pub fn interpret_ast(&mut self, ast: &[StmtContainer]) -> Result<(), Diagnostic> {
        for stmt in ast {
            if let Stmt::Fun(fs) = &stmt.stmt {
                if INTERNER.read().get_interned_string(fs.name) != "main" {
                    continue;
                }
            }

            self.interpret_stmt(stmt)?;
        }

        Ok(())
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
                            addr: interpreted_value,
                        },
                    );
            }
            Stmt::Fun(fun_stmt) => {
                self.interpret_function(fun_stmt)?;
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
                for stmt in stmts {
                    self.interpret_stmt(stmt)?;
                }
            }
            Stmt::If(if_stmt) => {
                let condition_addr = self.interpret_expr(&if_stmt.condition)?;
                let condition_container = self.allocator.get(condition_addr);
                let condition_read = condition_container.read();
                let is_condition_met = match &*condition_read {
                    Value::None | Value::Boolean(false) => false,
                    _ => true,
                };

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
                let iterable_addr = self.interpret_expr(&iter_stmt.iterable)?;
                let iterable_container = self.allocator.get(iterable_addr);
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
                            addr: *item,
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
                let val_addr = self.interpret_expr(v)?;

                return Err(Diagnostic {
                    severity: Severity::Error,
                    code: ErrorCode::Return(val_addr),
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
            _ => todo!("{:?}", stmt),
        }

        Ok(())
    }

    fn interpret_function(&mut self, fun_stmt: &FunStmt) -> Result<Option<ValueAddr>, Diagnostic> {
        let mut env = self.envs.borrow_mut();
        env.get_mut(&self.current).unwrap().push(HashMap::new());
        drop(env);

        match &fun_stmt.body.stmt {
            Stmt::Block(stmts) => {
                for stmt in stmts {
                    if let Err(err) = self.interpret_stmt(stmt) {
                        return if let ErrorCode::Return(rv) = err.code {
                            Ok(Some(rv))
                        } else {
                            Err(err)
                        };
                    }
                }
            }
            _ => unreachable!(),
        }

        let mut env = self.envs.borrow_mut();
        for _val in env.get_mut(&self.current).unwrap().last().unwrap() {
            // FIXME: make sure deallocation actually works. At this moment, this will deallocate even if there
            //        are "ValueAddr"s pointing to it
            // self.allocator.deallocate(val.1.addr);
        }
        Ok(None)
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

    fn interpret_expr(&mut self, expr: &ExprContainer) -> Result<ValueAddr, Diagnostic> {
        let val = match &expr.expr {
            Expr::Assignment(assignment_expr) => {
                let name = match assignment_expr.lvalue.expr {
                    Expr::Variable(name) => name,
                    _ => unreachable!(),
                };
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

                let rvalue = self.interpret_expr(&assignment_expr.rvalue)?;
                let mut envs = self.envs.borrow_mut();
                let scopes = envs.get_mut(&self.current).unwrap().iter_mut().rev();

                for scope in scopes {
                    if scope.get_mut(&name).is_some() {
                        scope.insert(
                            name,
                            EnvItem {
                                mutable: Mutable::Yes,
                                addr: rvalue,
                            },
                        );
                    }
                }

                return Ok(rvalue);
            }
            Expr::Data(data_expr) => {
                let data_addr = self.interpret_expr(&ExprContainer {
                    span: expr.span,
                    expr: Expr::Variable(data_expr.name),
                })?;
                let data_container = self.allocator.get(data_addr);
                let data_read = data_container.read();
                let data_stmt = match &*data_read {
                    Value::Data(ds, _) => ds,
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
                let lhs_addr = self.interpret_expr(&logical_expr.lhs)?;
                let lhs_container = self.allocator.get(lhs_addr);
                let lhs_inner = lhs_container.read();
                let rhs_addr = self.interpret_expr(&logical_expr.rhs)?;
                let rhs_container = self.allocator.get(rhs_addr);
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
                    (Value::Int(lhs), Value::Float(rhs)) | (Value::Float(rhs), Value::Int(lhs)) => {
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
                let lhs_addr = self.interpret_expr(&binary_expr.lhs)?;
                let lhs_container = self.allocator.get(lhs_addr);
                let lhs_inner = lhs_container.read();
                let rhs_addr = self.interpret_expr(&binary_expr.rhs)?;
                let rhs_container = self.allocator.get(rhs_addr);
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
                    (Value::Int(lhs), Value::Float(rhs)) | (Value::Float(rhs), Value::Int(lhs)) => {
                        match op {
                            BinaryOperation::Plus => Value::Float(*lhs as f64 + *rhs),
                            BinaryOperation::Minus => Value::Float(*lhs as f64 - *rhs),
                            BinaryOperation::Multiply => Value::Float(*lhs as f64 * *rhs),
                            BinaryOperation::Divide => Value::Float(*lhs as f64 / *rhs),
                            BinaryOperation::Modulus => Value::Float(*lhs as f64 % *rhs),
                            _ => return return_error(),
                        }
                    }
                    (Value::Float(lhs), Value::Float(rhs)) => {
                        match op {
                            BinaryOperation::Plus => Value::Float(*lhs + *rhs),
                            BinaryOperation::Minus => Value::Float(*lhs - *rhs),
                            BinaryOperation::Multiply => Value::Float(*lhs * *rhs),
                            BinaryOperation::Divide => Value::Float(*lhs / *rhs),
                            BinaryOperation::Modulus => Value::Float(*lhs % *rhs),
                            _ => return return_error(),
                        }
                    }
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
                        new_arr.push(rhs_addr);
                        let arr_addr = self.allocator.allocate(Value::Array(new_arr));

                        return Ok(arr_addr);
                    }
                    _ => return return_error(),
                };

                Ok(res)
            }
            Expr::Unary(unary_expr) => {
                let value_addr = self.interpret_expr(&unary_expr.expr)?;
                let value_container = self.allocator.get(value_addr);
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
                let function_addr = self.interpret_expr(&call_expr.callee)?;
                let function_container = self.allocator.get(function_addr);
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
                if function.arity() != usize::MAX && call_expr.args.len() > function.arity() {
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
                    args.push(self.allocator.clone(val));
                }

                let res = function.call(self, args);
                if let Ok(v) = res {
                    return Ok(v.unwrap_or(self.allocator.get_none()));
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
                let object_addr = self.interpret_expr(&get_expr.object)?;
                let object_container = self.allocator.get(object_addr);
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
                    return Ok(*value);
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
                let array_addr = self.interpret_expr(&index_expr.object)?;
                let array_container = self.allocator.get(array_addr);
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

                let index_addr = self.interpret_expr(&index_expr.index)?;
                let index = match &*self.allocator.get(index_addr).read() {
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

                return if let Some(val_addr) = array.get(index as usize) {
                    Ok(*val_addr)
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
                let array_addr = self.interpret_expr(&index_expr.object)?;
                let array_container = self.allocator.get(array_addr);

                let index_addr = self.interpret_expr(&index_expr.index)?;
                let index = match &*self.allocator.get(index_addr).read() {
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

                let value_addr = self.interpret_expr(&indexset_expr.value)?;
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

                array[index as usize] = value_addr;
                return Ok(value_addr);
            }
            Expr::Set(set_expr) => {
                if let Expr::Get(get_expr) = &set_expr.object.expr {
                    let value = self.interpret_expr(&set_expr.value)?;
                    let obj_addr = self.interpret_expr(&get_expr.object)?;
                    let obj_container = self.allocator.get(obj_addr);
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
                        e.insert(value);
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

                    return Ok(obj_addr);
                }

                let object_addr = self.interpret_expr(&set_expr.object)?;
                let value_addr = self.interpret_expr(&set_expr.value)?;
                let object_container = self.allocator.get(object_addr);
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
                object.values.insert(set_expr.property, value_addr);
                return Ok(object_addr);
            }
            Expr::Variable(name) => {
                let envs = self.envs.borrow_mut();
                let scopes = envs[&self.current].iter().rev();

                for scope in scopes {
                    if let Some(env_item) = scope.get(name) {
                        return Ok(env_item.addr);
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
                    addr: allocator.allocate(Value::Function(function)),
                };
                scope.insert(INTERNER.write().intern_string(name.to_string()), env_item);
                env_item
            };

        create_native_fn(allocator, "print", Box::new(PrintFunction {}));
        create_native_fn(allocator, "println", Box::new(PrintlnFunction {}));
        create_native_fn(allocator, "sleep", Box::new(SleepFunction {}));
        create_native_fn(allocator, "pop", Box::new(ArrayPopFunction {}));

        for stmt in ast {
            match &stmt.stmt {
                Stmt::Fun(fun_stmt) => {
                    scope.insert(
                        fun_stmt.name,
                        EnvItem {
                            mutable: Mutable::No,
                            addr: allocator.allocate(Value::Function(Box::new(YalFunction {
                                fun_stmt: fun_stmt.clone(),
                            }))),
                        },
                    );
                }
                Stmt::Data(data_stmt) => {
                    let value_addr =
                        allocator.allocate(Value::Data(data_stmt.clone(), HashMap::new()));
                    scope.insert(
                        data_stmt.name,
                        EnvItem {
                            mutable: Mutable::No,
                            addr: value_addr,
                        },
                    );
                }
                Stmt::Methods(methods_stmt) => {
                    let data_container = match scope.get(&methods_stmt.data) {
                        Some(ei) => allocator.get(ei.addr),
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

    pub fn get_allocator(&mut self) -> &mut Allocator {
        &mut self.allocator
    }
}

#[derive(Debug, Clone)]
struct YalFunction {
    fun_stmt: Box<FunStmt>,
}

impl Callable for YalFunction {
    fn arity(&self) -> usize {
        self.fun_stmt.arguments.len()
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<ValueAddr>,
    ) -> Result<Option<ValueAddr>, Diagnostic> {
        let mut env = interpreter.envs.borrow_mut();
        let mut scope = HashMap::new();
        for (k, v) in self.fun_stmt.arguments.iter().zip(args.iter()) {
            scope.insert(
                *k,
                EnvItem {
                    addr: *v,
                    mutable: Mutable::No,
                },
            );
        }
        env.get_mut(&interpreter.current).unwrap().push(scope);
        drop(env);

        match &self.fun_stmt.body.stmt {
            Stmt::Block(stmts) => {
                for stmt in stmts {
                    if let Err(err) = interpreter.interpret_stmt(stmt) {
                        return if let ErrorCode::Return(rv) = err.code {
                            Ok(Some(rv))
                        } else {
                            Err(err)
                        };
                    }
                }
            }
            _ => unreachable!(),
        }

        let mut env = interpreter.envs.borrow_mut();
        for _val in env.get_mut(&interpreter.current).unwrap().last().unwrap() {
            // FIXME: make sure deallocation actually works. At this moment, this will deallocate even if there
            //        are "ValueAddr"s pointing to it
            // self.allocator.deallocate(val.1.addr);
        }
        Ok(None)
    }

    fn to_string(&self) -> String {
        format!(
            "<function {} at {:p}>",
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

#[derive(Debug, Clone)]
enum InterpreterContext {
    Function,
    Loop,
    Method,
}

#[derive(Debug, Clone, Copy)]
pub struct EnvItem {
    pub mutable: Mutable,
    pub addr: ValueAddr,
}
