//! This crate wraps `coursier`, and its entire purpose it to just pull down the
//! scala standard library. I can't find a way to do this with BSP, and its a
//! lot easier to just shell out to `coursier` instead of dealing with BSP
//! anyway.

use std::path::PathBuf;

// TODO: Maybe bump the version at some point?
const VERSION: &str = "2.13.14";

pub fn sources_path() -> std::path::PathBuf {
  // FIXME: Make this less bad.
  let dir = PathBuf::from("/home/macmv/.cache/scalarc");

  // TODO: Bump version
  dir.join("sources").join(VERSION)
}

pub fn fetch_stdlib() -> Result<(), std::io::Error> {
  let output = std::process::Command::new("coursier")
    .arg("fetch")
    .arg(format!("org.scala-lang:scala-library:{}", VERSION))
    .arg("--classifier")
    .arg("sources")
    .output()?;

  if !output.status.success() {
    return Err(std::io::Error::new(
      std::io::ErrorKind::Other,
      format!("Failed to fetch scala stdlib: {}", String::from_utf8_lossy(&output.stderr)),
    ));
  }

  let path = String::from_utf8(output.stdout).unwrap();
  let path = path.trim();
  assert!(path.lines().count() == 1, "multiple sources found");

  let extract_path = sources_path();
  std::fs::create_dir_all(&extract_path)?;
  let output =
    std::process::Command::new("unzip").arg("-o").arg(path).current_dir(extract_path).output()?;

  if !output.status.success() {
    return Err(std::io::Error::new(
      std::io::ErrorKind::Other,
      format!("Failed to unzip scala stdlib: {}", String::from_utf8_lossy(&output.stderr)),
    ));
  }

  Ok(())
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_works() { fetch_stdlib().unwrap(); }
}
