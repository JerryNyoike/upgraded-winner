// Supported types for this Miranda

#[derive(Debug)]
pub enum MirandaExpr {
    MirandaBoolean(bool),
    MirandaNum(i32),
    MirandaChar(char),
    MirandaString(String),
    MirandaKeyword(Keyword),
    MirandaList(Vec<MirandaExpr>),
    MirandaFunction(MirandaFunc),
}

#[derive(Debug)]
enum MirandaFunc {
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

#[derive(Debug)]
pub enum Keyword {
    Where,
    If,
    Otherwise,
    Type,
}

#[derive(Debug)]
struct UserFunc {
    frame_id: u32,
}

struct Env {}
