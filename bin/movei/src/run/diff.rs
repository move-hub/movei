use movei_executor::change_set::{Change, ChangeSet};
use resource_viewer::MoveValueAnnotator;

pub fn print_diff(changes: ChangeSet, annotator: MoveValueAnnotator) -> anyhow::Result<()> {
    for (addr, cs) in changes.into_changes() {
        for c in cs {
            let (old, new) = match c {
                Change::DeleteResource(t) => (t.to_string(), String::new()),
                Change::ModifyResource(t, layout, new_data) => {
                    let new = {
                        let data = new_data.simple_serialize(&layout).unwrap();
                        let annotated_s = annotator.view_struct(t, data.as_slice())?;
                        format!("{}", annotated_s)
                    };
                    (String::new(), new)
                }
                Change::AddModule(m, _) => {
                    let new = format!("{:#x}::{}", m.address(), m.name());
                    (String::new(), new)
                }
            };

            println!("address {:#x}:", addr);
            let cs = difference::Changeset::new(&old, &new, "");
            println!("{}", &cs);
        }
    }
    Ok(())
}
