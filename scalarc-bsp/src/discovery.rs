use serde::Deserialize;
use std::{fs, path::Path};

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
#[allow(unused)]
pub struct BspJsonConfig {
  pub name:        String,
  pub version:     String,
  pub bsp_version: String,
  pub languages:   Vec<String>,
  pub argv:        Vec<String>,
}

pub fn find_bsp_servers(dir: &Path) -> Vec<BspJsonConfig> {
  let bsp_dir = dir.join(".bsp");

  let mut configs = vec![];

  let Ok(dir) = fs::read_dir(&bsp_dir) else { return vec![] };
  for file in dir {
    let Ok(file) = file else { return vec![] };
    let path = file.path();

    if path.is_file() {
      let Ok(file) = fs::read_to_string(&path) else { continue };
      let Ok(config) = serde_json::from_str(&file) else { continue };
      configs.push(config);
    }
  }

  configs
}
