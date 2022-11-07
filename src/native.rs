use crate::diagnostics::{Diagnostic, ErrorCode, Severity, Span};
use crate::interner::INTERNER;
use crate::interp::Interpreter;
use crate::memory::{Callable, Value};

use std::io::Write;
use std::sync::Arc;
use std::thread::sleep;
use std::time::Duration;

use parking_lot::RwLock;

#[derive(Debug, Clone)]
pub struct PrintFunction {}

impl PrintFunction {
    pub fn stringify_value(value: &Value) -> String {
        match value {
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Boolean(b) => b.to_string(),
            Value::String(s) => INTERNER.read().get_interned_string(*s).to_string(),
            Value::None => "None".to_string(),
            Value::Function(f) => f.to_string(),
            Value::Data(_, _) => "<data>".to_string(),
            Value::DataObject(_) => format!("<data object at 0x{:p}>", value as *const _),
            Value::Array(a) => {
                let mut res = String::from("[");
                let mut str_vals = vec![];
                for v in a {
                    let val = v.clone();
                    let val_reader = val.read();
                    if let Value::String(s) = &*val_reader {
                        str_vals.push(format!("{:?}", INTERNER.read().get_interned_string(*s)));
                    } else {
                        str_vals.push(Self::stringify_value(&val_reader));
                    }
                }

                res.push_str(&str_vals.join(", "));
                res.push(']');
                res
            }
        }
    }
}

impl Callable for PrintFunction {
    fn arity(&self) -> usize {
        usize::MAX
    }

    fn call(
        &self,
        _interpreter: &mut Interpreter,
        args: Vec<Arc<RwLock<Value>>>,
    ) -> Result<Option<Arc<RwLock<Value>>>, Diagnostic> {
        for arg in args {
            let value_read = arg.read();
            print!("{}", Self::stringify_value(&value_read));
            std::io::stdout().flush().unwrap();
        }

        Ok(None)
    }

    fn to_string(&self) -> String {
        format!("<native function `print` at {:p}>", self as *const _)
    }

    fn clone(&self) -> Self
    where
        Self: Sized,
    {
        Clone::clone(self)
    }
}

#[derive(Debug, Clone)]
pub struct PrintlnFunction {}

impl Callable for PrintlnFunction {
    fn arity(&self) -> usize {
        usize::MAX
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Arc<RwLock<Value>>>,
    ) -> Result<Option<Arc<RwLock<Value>>>, Diagnostic> {
        PrintFunction {}.call(interpreter, args)?;
        println!();
        std::io::stdout().flush().unwrap();
        Ok(None)
    }

    fn to_string(&self) -> String {
        format!("<native function `println` at {:p}>", self as *const _)
    }

    fn clone(&self) -> Self
    where
        Self: Sized,
    {
        Clone::clone(self)
    }
}

#[derive(Debug, Clone)]
pub struct SleepFunction {}

impl Callable for SleepFunction {
    fn arity(&self) -> usize {
        1
    }

    fn call(
        &self,
        _interpreter: &mut Interpreter,
        args: Vec<Arc<RwLock<Value>>>,
    ) -> Result<Option<Arc<RwLock<Value>>>, Diagnostic> {
        let time_container = args[0].clone();
        let time_read = time_container.read();
        let time = match &*time_read {
            Value::Int(i) => *i as f64,
            Value::Float(f) => *f,
            val => {
                return Err(Diagnostic {
                    severity: Severity::Error,
                    code: ErrorCode::InvalidType,
                    message: format!(
                        "Expected type `int` or `float`, found type `{}`",
                        val.type_()
                    ),
                    span: Span {
                        lo: usize::MAX,
                        hi: usize::MAX,
                    },
                })
            }
        };

        sleep(Duration::from_secs_f64(time));

        Ok(None)
    }

    fn to_string(&self) -> String {
        format!("<native function `sleep` at {:p}>", self as *const _)
    }

    fn clone(&self) -> Self
    where
        Self: Sized,
    {
        Clone::clone(self)
    }
}

#[derive(Debug, Clone)]
pub struct ArrayPopFunction {}

impl Callable for ArrayPopFunction {
    fn arity(&self) -> usize {
        1
    }

    fn call(
        &self,
        _interpreter: &mut Interpreter,
        args: Vec<Arc<RwLock<Value>>>,
    ) -> Result<Option<Arc<RwLock<Value>>>, Diagnostic> {
        let array_container = args[0].clone();
        let mut array_read = array_container.write();
        let array = match &mut *array_read {
            Value::Array(arr) => arr,
            val => {
                return Err(Diagnostic {
                    severity: Severity::Error,
                    code: ErrorCode::InvalidType,
                    message: format!("Expected type `array`, found type `{}`", val.type_()),
                    span: Span {
                        lo: usize::MAX,
                        hi: usize::MAX,
                    },
                })
            }
        };
        Ok(array.pop())
    }

    fn to_string(&self) -> String {
        format!("<native function `sleep` at {:p}>", self as *const _)
    }

    fn clone(&self) -> Self
    where
        Self: Sized,
    {
        Clone::clone(self)
    }
}

#[derive(Debug, Clone)]
pub struct InputFunction {}

impl Callable for InputFunction {
    fn arity(&self) -> usize {
        usize::MAX
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Arc<RwLock<Value>>>,
    ) -> Result<Option<Arc<RwLock<Value>>>, Diagnostic> {
        if !args.is_empty() {
            PrintFunction {}.call(interpreter, args)?;
        }

        let mut line = String::new();
        loop {
            let size = std::io::stdin().read_line(&mut line).unwrap();
            if size > 1 {
                break;
            }
        }
        line.pop();

        Ok(Some(interpreter.get_allocator_mut().allocate(
            Value::String(INTERNER.write().intern_string(line)),
        )))
    }

    fn to_string(&self) -> String {
        format!("<native function `input` at {:p}>", self as *const _)
    }

    fn clone(&self) -> Self
    where
        Self: Sized,
    {
        Clone::clone(self)
    }
}

#[derive(Debug, Clone)]
pub struct IntFunction {}

impl Callable for IntFunction {
    fn arity(&self) -> usize {
        1
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Arc<RwLock<Value>>>,
    ) -> Result<Option<Arc<RwLock<Value>>>, Diagnostic> {
        let n_container = args[0].clone();
        let n_read = n_container.read();
        let res: i64 = match &*n_read {
            Value::String(s) => match INTERNER.read().get_interned_string(*s).parse() {
                Ok(res) => res,
                Err(e) => {
                    return Err(Diagnostic {
                        span: Span {
                            lo: usize::MAX,
                            hi: usize::MAX,
                        },
                        code: ErrorCode::InvalidNumber,
                        message: format!("Failed to parse number: {e}"),
                        severity: Severity::Error,
                    })
                }
            },
            Value::Float(f) => *f as _,
            Value::Int(i) => *i,
            _ => {
                return Err(Diagnostic {
                    span: Span {
                        lo: usize::MAX,
                        hi: usize::MAX,
                    },
                    code: ErrorCode::InvalidType,
                    message: format!(
                        "Expected type `string`, `float` or `int`, found {}",
                        n_read.type_()
                    ),
                    severity: Severity::Error,
                })
            }
        };

        Ok(Some(
            interpreter.get_allocator_mut().allocate(Value::Int(res)),
        ))
    }

    fn to_string(&self) -> String {
        format!("<native function `int` at {:p}>", self as *const _)
    }

    fn clone(&self) -> Self
    where
        Self: Sized,
    {
        Clone::clone(self)
    }
}

#[derive(Debug, Clone)]
pub struct FloatFunction {}

impl Callable for FloatFunction {
    fn arity(&self) -> usize {
        1
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Arc<RwLock<Value>>>,
    ) -> Result<Option<Arc<RwLock<Value>>>, Diagnostic> {
        let n_container = args[0].clone();
        let n_read = n_container.read();
        let res: f64 = match &*n_read {
            Value::String(s) => match INTERNER.read().get_interned_string(*s).parse() {
                Ok(res) => res,
                Err(e) => {
                    return Err(Diagnostic {
                        span: Span {
                            lo: usize::MAX,
                            hi: usize::MAX,
                        },
                        code: ErrorCode::InvalidNumber,
                        message: format!("Failed to parse number: {e}"),
                        severity: Severity::Error,
                    })
                }
            },
            Value::Float(f) => *f,
            Value::Int(i) => *i as _,
            _ => {
                return Err(Diagnostic {
                    span: Span {
                        lo: usize::MAX,
                        hi: usize::MAX,
                    },
                    code: ErrorCode::InvalidType,
                    message: format!(
                        "Expected type `string`, `float` or `int`, found {}",
                        n_read.type_()
                    ),
                    severity: Severity::Error,
                })
            }
        };

        Ok(Some(
            interpreter.get_allocator_mut().allocate(Value::Float(res)),
        ))
    }

    fn to_string(&self) -> String {
        format!("<native function `float` at {:p}>", self as *const _)
    }

    fn clone(&self) -> Self
    where
        Self: Sized,
    {
        Clone::clone(self)
    }
}

#[derive(Debug, Clone)]
pub struct PowFunction {}

impl Callable for PowFunction {
    fn arity(&self) -> usize {
        2
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Arc<RwLock<Value>>>,
    ) -> Result<Option<Arc<RwLock<Value>>>, Diagnostic> {
        let v_1_container = args[0].clone();
        let v_1_read = v_1_container.read();
        let v_1 = match &*v_1_read {
            Value::Int(i) => *i as f64,
            Value::Float(f) => *f,
            _ => {
                return Err(Diagnostic {
                    span: Span {
                        lo: usize::MAX,
                        hi: usize::MAX,
                    },
                    code: ErrorCode::InvalidType,
                    message: format!(
                        "Expected type `string`, `float` or `int`, found {}",
                        v_1_read.type_()
                    ),
                    severity: Severity::Error,
                })
            }
        };

        let v_2_container = args[1].clone();
        let v_2_read = v_2_container.read();
        let v_2 = match &*v_2_read {
            Value::Int(i) => *i as f64,
            Value::Float(f) => *f,
            _ => {
                return Err(Diagnostic {
                    span: Span {
                        lo: usize::MAX,
                        hi: usize::MAX,
                    },
                    code: ErrorCode::InvalidType,
                    message: format!(
                        "Expected type `string`, `float` or `int`, found {}",
                        v_2_read.type_()
                    ),
                    severity: Severity::Error,
                })
            }
        };

        Ok(Some(
            interpreter
                .get_allocator_mut()
                .allocate(Value::Float(v_1.powf(v_2))),
        ))
    }

    fn to_string(&self) -> String {
        format!("<native function `pow` at {:p}>", self as *const _)
    }

    fn clone(&self) -> Self
    where
        Self: Sized,
    {
        Clone::clone(self)
    }
}
