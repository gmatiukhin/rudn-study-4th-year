use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
struct Pc {
    r#type: Type,
    role: Role,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
enum Type {
    Type1,
    Type2,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
enum Role {
    Ddns,
    Dns,
    Gateway,
    Router,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
enum Test {
    Test1,
    Test2,
    Test3,
    Test4,
    Test5,
}
