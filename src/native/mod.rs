use crate::diagnostics::{Diagnostic, ErrorCode, Severity, Span};
use crate::interner::INTERNER;
use crate::memory::{Allocator, Value};
use crate::memory::{Callable, ValueAddr};
use crate::Interpreter;

use std::io::{Read, Write};
use std::thread::sleep;
use std::time::Duration;

#[derive(Debug, Clone)]
pub struct PrintFunction {}

impl PrintFunction {
    pub fn stringify_value(allocator: &Allocator, value: &Value) -> String {
        match &*value {
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
                    let val = allocator.get(*v);
                    let val_reader = val.read();
                    if let Value::String(s) = &*val_reader {
                        str_vals.push(format!("{:?}", INTERNER.read().get_interned_string(*s)));
                    } else {
                        str_vals.push(Self::stringify_value(allocator, &*val_reader));
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
        interpreter: &mut Interpreter,
        args: Vec<ValueAddr>,
    ) -> Result<Option<ValueAddr>, Diagnostic> {
        for arg in args {
            let value = interpreter.get_allocator_mut().get(arg);
            let value_read = value.read();
            print!(
                "{}",
                Self::stringify_value(interpreter.get_allocator_mut(), &*value_read)
            );
            std::io::stdout().flush().unwrap();
        }

        Ok(None)
    }

    fn to_string(&self) -> String {
        format!("<native function \"print\" at {:p}>", self as *const _)
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
        args: Vec<ValueAddr>,
    ) -> Result<Option<ValueAddr>, Diagnostic> {
        PrintFunction {}.call(interpreter, args)?;
        println!();
        std::io::stdout().flush().unwrap();
        Ok(None)
    }

    fn to_string(&self) -> String {
        format!("<native function \"println\" at {:p}>", self as *const _)
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
        interpreter: &mut Interpreter,
        args: Vec<ValueAddr>,
    ) -> Result<Option<ValueAddr>, Diagnostic> {
        let time_addr = args[0];
        let time_container = interpreter.get_allocator_mut().get(time_addr);
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
        format!("<native function \"sleep\" at {:p}>", self as *const _)
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
        interpreter: &mut Interpreter,
        args: Vec<ValueAddr>,
    ) -> Result<Option<ValueAddr>, Diagnostic> {
        let array_addr = args[0];
        let array_container = interpreter.get_allocator_mut().get(array_addr);
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
        format!("<native function \"sleep\" at {:p}>", self as *const _)
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
        args: Vec<ValueAddr>,
    ) -> Result<Option<ValueAddr>, Diagnostic> {
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
        format!("<native function \"input\" at {:p}>", self as *const _)
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
        args: Vec<ValueAddr>,
    ) -> Result<Option<ValueAddr>, Diagnostic> {
        let n_s = args[0];
        let n_container = interpreter.get_allocator_mut().get(n_s);
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
        format!("<native function \"input\" at {:p}>", self as *const _)
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
        args: Vec<ValueAddr>,
    ) -> Result<Option<ValueAddr>, Diagnostic> {
        let n_s = args[0];
        let n_container = interpreter.get_allocator_mut().get(n_s);
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
        format!("<native function \"input\" at {:p}>", self as *const _)
    }

    fn clone(&self) -> Self
    where
        Self: Sized,
    {
        Clone::clone(self)
    }
}
