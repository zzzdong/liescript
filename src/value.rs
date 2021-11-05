use std::{any::Any, collections::HashMap, fmt, sync::Arc};

#[derive(Debug)]
pub enum Value {
    Bool(bool),
    Byte(u8),
    Int(i64),
    Float(f64),
    String(String),
    Array(Vec<Value>),
    Map(HashMap<String, Value>),
    Object(Box<dyn Object>),
    Undefined,
}

pub trait Object: fmt::Display + fmt::Debug + Any + Sync + Send {
    fn get_attr(&self, name: &str) -> Option<Value> {
        let _name = name;
        None
    }
}
