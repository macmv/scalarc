#[cfg(test)]
mod tests;

pub mod tree;

// TODO: Implement.
#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase {}
