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

    pub fn get_none(&self) -> Arc<RwLock<Value>> {
        self.allocations[0].clone()
    }

    pub fn allocate(&mut self, value: Value) -> Arc<RwLock<Value>> {
        if self.allocations.is_empty() {
            self.allocations.push(Arc::new(RwLock::new(Value::None)));
        }
        if matches!(value, Value::None) {
            return self.allocations[0].clone();
        }
        self.allocations.push(Arc::new(RwLock::new(value)));
        self.allocations.last().unwrap().clone()
    }

    pub fn clone(&mut self, value: &Arc<RwLock<Value>>) -> Arc<RwLock<Value>> {
        let value_read = value.read();
        let cloned_value = match &*value_read {
            Value::Int(i) => Value::Int(*i),
            Value::Float(f) => Value::Float(*f),
            Value::Boolean(b) => Value::Boolean(*b),
            Value::String(s) => Value::String(*s),
            Value::None => Value::None,
            Value::Function(_f) => return value.clone(),
            Value::Data(d, m) => Value::Data(d.clone(), m.clone()),
            Value::DataObject(d_object) => Value::DataObject(d_object.clone()),
            Value::Array(a) => Value::Array(a.clone()),
        };
        self.allocations.push(Arc::new(RwLock::new(cloned_value)));
        self.allocations.last().unwrap().clone()
    }

    pub fn dealloc_unused_objects(&mut self) {
        self.allocations.retain(|e| Arc::strong_count(e) > 1)
    }
}

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
    Array(Vec<Arc<RwLock<Value>>>),
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
        args: Vec<Arc<RwLock<Value>>>,
    ) -> Result<Option<Arc<RwLock<Value>>>, Diagnostic>;
    fn to_string(&self) -> String;
    fn clone(&self) -> Self
    where
        Self: Sized;
}

#[derive(Debug, Clone)]
pub struct DataObject {
    pub name: InternedString,
    pub values: HashMap<InternedString, Arc<RwLock<Value>>>,
    pub methods: HashMap<InternedString, Box<FunStmt>>,
}
