use crate::DefId;

#[derive(Debug, Eq, PartialEq)]
pub struct Body {}

impl Body {
  pub fn new(db: &dyn crate::HirDatabase, def: DefId) -> Self {
    let loc = def.lookup(db);
    let file = db.file_package(loc.container);
    let def = &file.arenas.def[loc.id];
    dbg!(&def);

    Body {}
  }
}
