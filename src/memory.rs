use crate::interner::InternedString;

use std::collections::HashMap;
use std::fmt::Debug;

use crate::ast::{DataStmt, FunStmt};
use crate::diagnostics::Diagnostic;
use crate::Interpreter;
use parking_lot::RwLock;
use std::sync::Arc;

pub struct Allocator {
    allocations: Vec<Arc<RwLock<Value>>>,
}

impl Allocator {
    pub const fn new() -> Self {
        Self {
            allocations: vec![],
        }
    }

    pub fn get_none(&self) -> ValueAddr {
        ValueAddr(0)
    }

    pub fn allocate(&mut self, value: Value) -> ValueAddr {
        if self.allocations.is_empty() {
            self.allocations.push(Arc::new(RwLock::new(Value::None)));
        }
        if matches!(value, Value::None) {
            return ValueAddr(0);
        }
        self.allocations.push(Arc::new(RwLock::new(value)));
        ValueAddr(self.allocations.len() - 1)
    }

    pub fn clone(&mut self, addr: ValueAddr) -> ValueAddr {
        let value_arc = self.allocations[addr.0].clone();
        let value_read = value_arc.read();
        let cloned_value = match &*value_read {
            Value::Int(i) => Value::Int(*i),
            Value::Float(f) => Value::Float(*f),
            Value::Boolean(b) => Value::Boolean(*b),
            Value::String(s) => Value::String(*s),
            Value::None => Value::None,
            Value::Function(_f) => return addr,
            Value::Data(d, m) => Value::Data(d.clone(), m.clone()),
            Value::DataObject(d_object) => Value::DataObject(d_object.clone()),
            Value::Array(a) => Value::Array(a.clone()),
        };
        self.allocations.push(Arc::new(RwLock::new(cloned_value)));
        ValueAddr(self.allocations.len() - 1)
    }

    pub fn deallocate(&mut self, addr: ValueAddr) {
        // FIXME: this is probably not the proper way to deallocate as currently references
        //        are stored as Valuaddr that doesn't hold any strong or weak pointer to value
        if let Some(obj) = self.allocations.get(addr.0) {
            if Arc::strong_count(obj) == 0 {
                self.allocations.remove(addr.0);
            }
        }
    }

    pub fn get(&self, addr: ValueAddr) -> Arc<RwLock<Value>> {
        self.allocations[addr.0].clone()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ValueAddr(usize);

#[derive(Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    Boolean(bool),
    String(InternedString),
    None,
    Function(Box<dyn Callable>),
    Data(Box<DataStmt>, HashMap<InternedString, Box<FunStmt>>),
    DataObject(DataObject),
    Array(Vec<ValueAddr>),
}

impl Value {
    pub fn type_(&self) -> String {
        String::from(match self {
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::Boolean(_) => "boolean",
            Value::String(_) => "string",
            Value::None => "none",
            Value::Function(_) => "function",
            Value::DataObject(_) => "data object",
            Value::Data(_, _) => "data",
            Value::Array(_) => "array",
        })
    }
}

pub trait Callable: Debug + Send + Sync {
    fn arity(&self) -> usize;
    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<ValueAddr>,
    ) -> Result<Option<ValueAddr>, Diagnostic>;
    fn to_string(&self) -> String;
    fn clone(&self) -> Self
    where
        Self: Sized;
}

#[derive(Debug, Clone)]
pub struct DataObject {
    pub name: InternedString,
    pub values: HashMap<InternedString, ValueAddr>,
    pub methods: HashMap<InternedString, Box<FunStmt>>,
}
