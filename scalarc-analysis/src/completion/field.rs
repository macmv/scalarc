use super::{Completer, Completion, CompletionKind};
use scalarc_hir::{
  hir::{self, AstId, BlockId},
  DefinitionKey, GlobalDefinition, GlobalDefinitionKind, HirDatabase, InFileExt, Type,
};
use scalarc_source::{SourceDatabase, TargetId};
use std::collections::HashSet;

impl Completer<'_> {
  pub fn field_completions(&self, target: TargetId, ty: Type) -> Vec<Completion> {
    let key = match ty {
      Type::Object(ref path) => DefinitionKey::Object(path.clone()),
      Type::Instance(ref path) => DefinitionKey::Instance(path.clone()),
      _ => return vec![],
    };

    for target in self.db.workspace().all_dependencies(target) {
      let defs = self.db.definitions_for_target(target);

      if let Some(def) = defs.items.get(&key) {
        return self.fields_of_def(def);
      }
    }

    vec![]
  }

  fn fields_of_def(&self, def: &GlobalDefinition) -> Vec<Completion> {
    let mut completions = vec![];

    // This is a bit of a mess, but effectively:
    // - Scopes are global "things," like classes and objects.
    // - Blocks are HIR blocks, which include `val`s and `def`s, and the result of
    //   typechecking.
    //
    // We need to collect both separately, as scopes don't exist in HIR land, and we
    // want type inference for vals/defs.
    self.collect_scope_fields_of_def(def, &mut completions);
    self.collect_block_fields_of_def(def, &mut completions);

    completions
  }

  fn collect_scope_fields_of_def(&self, def: &GlobalDefinition, completions: &mut Vec<Completion>) {
    let body = match def.kind {
      GlobalDefinitionKind::Class(Some(body_id), _) => body_id,
      GlobalDefinitionKind::Object(Some(body_id)) => body_id,
      _ => return,
    };

    let scopes = self.db.scopes_of(def.file_id);
    let Some(scope) = scopes.get(body) else { return };

    let mut names = HashSet::new();
    for (_, def) in &scope.declarations {
      if names.insert(def.name.clone()) {
        completions.push(Completion {
          label: def.name.as_str().into(),
          kind:  CompletionKind::Global(def.kind.clone()),
        });
      }
    }
  }

  fn collect_block_fields_of_def(&self, def: &GlobalDefinition, completions: &mut Vec<Completion>) {
    let block_id = match def.kind {
      GlobalDefinitionKind::Class(Some(_), _) => BlockId::Class(AstId::new(def.ast_id)),
      GlobalDefinitionKind::Object(Some(_)) => BlockId::Object(AstId::new(def.ast_id)),
      _ => return,
    };

    let block = self.db.hir_ast_for_block(block_id.in_file(def.file_id));

    let mut names = HashSet::new();
    for item in &block.items {
      match block.stmts[*item] {
        hir::Stmt::Binding(ref binding) => {
          if names.insert(binding.name.clone()) {
            completions.push(Completion {
              label: binding.name.as_str().into(),
              kind:  CompletionKind::Hir(scalarc_hir::HirDefinitionKind::Val(None)),
            });
          }
        }
        _ => {}
      }
    }
  }
}
