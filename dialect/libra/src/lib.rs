use dialect::MoveDialect;

#[derive(Clone, Debug, Copy)]
pub struct LibraDialect;
const NAME: &str = "Libra";
impl MoveDialect for LibraDialect {
    fn name(&self) -> &str {
        NAME
    }
    fn stdlib_files(&self) -> Vec<String> {
        vec![]
    }
}
