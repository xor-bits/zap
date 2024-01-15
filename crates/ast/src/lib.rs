// pub struct Ast {
//     root: Root,
// }

pub struct Root {
    inner: Vec<RootItem>,
}

pub enum RootItem {
    Const,
    Func,
    Test,
}

//
