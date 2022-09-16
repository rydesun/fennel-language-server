use std::collections::HashSet;

fn main() {
    let file = std::env::args().nth(1).unwrap();
    let contents = std::fs::read_to_string(file).unwrap();
    let ast = fennel_parser::parse(&contents, HashSet::new());
    ast.errors().for_each(|e| eprintln!("diagnostic: {:#?}", e))
}
