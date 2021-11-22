// Supported types for this Miranda

#[derive(Debug, PartialEq)]
pub enum MirandaExpr {
    MirandaBoolean(bool),
    MirandaNum(i32),
    MirandaFloat(f32),
    MirandaChar(char),
    MirandaString(String),
    MirandaKeyword(Keyword),
    MirandaList(Vec<MirandaExpr>),
    MirandaFunction(MirandaFunc),
    MirandaIdentifier(String),
}

#[derive(Debug, PartialEq)]
pub enum MirandaFunc {
    UserDefined(UserFunc),
    CoreFunc,
}

#[derive(Debug)]
pub enum BuiltIn {
    Plus,
    Minus,
    Times,
    Divide,
    Equal,
    Mod,
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Where,
    If,
    Otherwise,
    Type,
}

#[derive(Debug, PartialEq)]
pub struct UserFunc {
    frame_id: u32,
}

pub struct Env {}
