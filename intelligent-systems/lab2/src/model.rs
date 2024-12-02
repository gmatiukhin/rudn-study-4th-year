use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use serde::{Deserialize, Serialize};
use structmap::ToMap;
use structmap_derive::ToMap;

#[derive(Debug, Clone, Copy, Serialize, Deserialize, ToMap)]
pub struct Pc {
    pub r#type: Type,
    pub role: Role,
}

impl Default for Pc {
    fn default() -> Self {
        Self {
            r#type: Type::Type1,
            role: Role::Dns,
        }
    }
}

impl From<Pc> for zen_engine::Variable {
    fn from(value: Pc) -> Self {
        let map = Pc::to_stringmap(value)
            .into_iter()
            .map(|(k, v)| (k, Self::String(v.as_str().into())))
            .collect::<HashMap<_, _, _>>();
        Self::Object(Rc::new(RefCell::new(map)))
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Type {
    Type1,
    Type2,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Role {
    Dns,
    Ddns,
    Gateway,
    Router,
}

impl Display for Role {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Test {
    Test1,
    Test2,
    Test3,
    Test4,
    Test5,
}
