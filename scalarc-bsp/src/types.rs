use lsp_types::Url;
use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct InitializeBuildParams {
  /// Name of the client.
  pub display_name: String,

  /// The version of the client.
  pub version: String,

  /// The BSP version that the client speaks.
  pub bsp_version: String,

  /// The rootUri of the workspace.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub root_uri: Option<Url>,

  /// The capabilities of the client.
  pub capabilities: BuildClientCapabilities,

  /// Additional metadata about the client.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub data: Option<Value>,
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct BuildClientCapabilities {
  /// The languages that this client supports.
  /// The ID strings for each language is defined in the LSP.
  /// The server must never respond with build targets for other
  /// languages than those that appear in this list.
  pub language_ids: Vec<String>,
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct InitializeBuildResult {
  /// Name of the server.
  display_name: String,

  /// The version of the server.
  version: String,

  /// The BSP version that the server speaks.
  bsp_version: String,

  /// The capabilities of the build server.
  capabilities: BuildServerCapabilities,
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct BuildServerCapabilities {
  /// The languages the server supports compilation via method
  /// buildTarget/compile.
  #[serde(skip_serializing_if = "Option::is_none")]
  compile_provider: Option<CompileProvider>,

  /// The languages the server supports test execution via method
  /// buildTarget/test.
  #[serde(skip_serializing_if = "Option::is_none")]
  test_provider: Option<TestProvider>,

  /// The languages the server supports run via method buildTarget/run.
  #[serde(skip_serializing_if = "Option::is_none")]
  run_provider: Option<RunProvider>,

  /// The languages the server supports debugging via method
  /// debugSession/start.
  #[serde(skip_serializing_if = "Option::is_none")]
  debug_provider: Option<DebugProvider>,

  /// The server can provide a list of targets that contain a
  /// single text document via the method buildTarget/inverseSources.
  #[serde(skip_serializing_if = "Option::is_none")]
  inverse_sources_provider: Option<bool>,

  /// The server provides sources for library dependencies
  /// via method buildTarget/dependencySources.
  #[serde(skip_serializing_if = "Option::is_none")]
  dependency_sources_provider: Option<bool>,

  /// The server can provide a list of dependency modules (libraries with
  /// meta information) via method buildTarget/dependencyModules.
  #[serde(skip_serializing_if = "Option::is_none")]
  dependency_modules_provider: Option<bool>,

  /// The server provides all the resource dependencies
  /// via method buildTarget/resources.
  #[serde(skip_serializing_if = "Option::is_none")]
  resources_provider: Option<bool>,

  /// The server provides all output paths
  /// via method buildTarget/outputPaths.
  #[serde(skip_serializing_if = "Option::is_none")]
  output_paths_provider: Option<bool>,

  /// The server sends notifications to the client on build
  /// target change events via buildTarget/didChange.
  #[serde(skip_serializing_if = "Option::is_none")]
  build_target_changed_provider: Option<bool>,

  /// The server can respond to `buildTarget/jvmRunEnvironment` requests with
  /// the necessary information required to launch a Java process to run a
  /// main class.
  #[serde(skip_serializing_if = "Option::is_none")]
  jvm_run_environment_provider: Option<bool>,

  /// The server can respond to `buildTarget/jvmTestEnvironment` requests
  /// with the necessary information required to launch a Java process for
  /// testing or debugging.
  #[serde(skip_serializing_if = "Option::is_none")]
  jvm_test_environment_provider: Option<bool>,

  /// The server can respond to `workspace/cargoFeaturesState` and
  /// `setCargoFeatures` requests. In other words, supports Cargo Features
  /// extension.
  #[serde(skip_serializing_if = "Option::is_none")]
  cargo_features_provider: Option<bool>,

  /// Reloading the build state through workspace/reload is supported.
  #[serde(skip_serializing_if = "Option::is_none")]
  can_reload: Option<bool>,
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct CompileProvider {
  pub language_ids: Vec<String>,
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct TestProvider {
  pub language_ids: Vec<String>,
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct RunProvider {
  pub language_ids: Vec<String>,
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct DebugProvider {
  pub language_ids: Vec<String>,
}
