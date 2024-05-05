use lsp_types::Url;
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use serde_json::Value;
use serde_repr::{Deserialize_repr, Serialize_repr};

pub trait BspRequest: Serialize {
  const METHOD: &'static str;

  type Result: DeserializeOwned;
}

pub trait BspNotification: Serialize + DeserializeOwned {
  const METHOD: &'static str;
}

impl BspRequest for InitializeBuildParams {
  const METHOD: &'static str = "build/initialize";

  type Result = InitializeBuildResult;
}

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

impl BspRequest for WorkspaceBuildTargetsRequest {
  const METHOD: &'static str = "workspace/buildTargets";

  type Result = WorkspaceBuildTargetsResult;
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct WorkspaceBuildTargetsRequest;

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct WorkspaceBuildTargetsResult {
  /// The build targets in this workspace that
  /// contain sources with the given language ids.
  pub targets: Vec<BuildTarget>,
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct BuildTarget {
  /// The target’s unique identifier.
  pub id: BuildTargetIdentifier,

  /// A human readable name for this target.
  /// May be presented in the user interface.
  /// Should be unique if possible.
  /// The id.uri is used if None.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub display_name: Option<String>,

  /// The directory where this target belongs to. Multiple build targets are
  /// allowed to map to the same base directory, and a build target is not
  /// required to have a base directory. A base directory does not
  /// determine the sources of a target, see buildTarget/sources.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub base_directory: Option<Url>,

  /// Free-form string tags to categorize or label this build target.
  /// For example, can be used by the client to:
  /// - customize how the target should be translated into the client's project
  ///   model.
  /// - group together different but related targets in the user interface.
  /// - display icons or colors in the user interface.
  /// Pre-defined tags are listed in `BuildTargetTag` but clients and servers
  /// are free to define new tags for custom purposes. */
  #[serde(skip_serializing_if = "Vec::is_empty")]
  pub tags: Vec<BuildTargetTag>,

  /** The set of languages that this target contains.
   * The ID string for each language is defined in the LSP. */
  #[serde(skip_serializing_if = "Vec::is_empty")]
  pub language_ids: Vec<String>,

  /** The direct upstream build target dependencies of this build target */
  #[serde(skip_serializing_if = "Vec::is_empty")]
  pub dependencies: Vec<BuildTargetIdentifier>,

  /** The capabilities of this build target. */
  pub capabilities: BuildTargetCapabilities,
}

/// A unique identifier for a target, can use any URI-compatible encoding as
/// long as it is unique within the workspace. Clients should not infer metadata
/// out of the URI structure such as the path or query parameters, use
/// BuildTarget instead.
#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct BuildTargetIdentifier {
  /** The target’s Uri */
  pub uri: Option<Url>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub enum BuildTargetTag {
  /// Target contains source code for producing any kind of application, may
  /// have but does not require the `canRun` capability.
  #[serde(rename = "application")]
  Application,

  /// Target contains source code to measure performance of a program, may
  /// have but does not require the `canRun` build target capability.
  #[serde(rename = "benchmark")]
  Benchmark,

  /// Target contains source code for integration testing purposes, may have
  /// but does not require the `canTest` capability.
  /// The difference between "test" and "integration-test" is that
  /// integration tests traditionally run slower compared to normal tests
  /// and require more computing resources to execute. */
  #[serde(rename = "integration-test")]
  IntegrationTest,

  /// Target contains re-usable functionality for downstream targets. May
  /// have any combination of capabilities.
  #[serde(rename = "library")]
  Library,

  /// Actions on the target such as build and test should only be invoked
  /// manually and explicitly. For example, triggering a build on all
  /// targets in the workspace should by default not include this target.
  /// The original motivation to add the "manual" tag comes from a similar
  /// functionality that exists in Bazel, where targets with this tag have
  /// to be specified explicitly on the command line.
  #[serde(rename = "manual")]
  Manual,

  /// Target should be ignored by IDEs.
  #[serde(rename = "no-ide")]
  NoIde,

  /// Target contains source code for testing purposes, may have but does not
  /// require the `canTest` capability.
  #[serde(rename = "test")]
  Test,
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct BuildTargetCapabilities {
  /// This target can be compiled by the BSP server.
  #[serde(skip_serializing_if = "Option::is_none")]
  can_compile: Option<bool>,

  /// This target can be tested by the BSP server.
  #[serde(skip_serializing_if = "Option::is_none")]
  can_test: Option<bool>,

  /// This target can be run by the BSP server.
  #[serde(skip_serializing_if = "Option::is_none")]
  can_run: Option<bool>,

  /// This target can be debugged by the BSP server.
  #[serde(skip_serializing_if = "Option::is_none")]
  can_debug: Option<bool>,
}

impl BspRequest for SourcesParams {
  const METHOD: &'static str = "buildTarget/sources";

  type Result = SourcesResult;
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct SourcesParams {
  pub targets: Vec<BuildTargetIdentifier>,
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct SourcesResult {
  pub items: Vec<SourcesItem>,
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct SourcesItem {
  pub target: BuildTargetIdentifier,

  /// The text documents or and directories that belong to this build target.
  pub sources: Vec<SourceItem>,

  /// The root directories from where source files should be relativized.
  /// Example: `file://Users/name/dev/metals/src/main/scala`
  pub roots: Option<Vec<Url>>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct SourceItem {
  /// Either a text document or a directory. A directory entry must end with a
  /// forward slash "/" and a directory entry implies that every nested text
  /// document within the directory belongs to this source item.
  pub uri: Url,

  /// Type of file of the source item, such as whether it is file or
  /// directory.
  pub kind: SourceItemKind,

  /// Indicates if this source is automatically generated by the build and is
  /// not intended to be manually edited by the user.
  pub generated: bool,
}

#[derive(Debug, Serialize_repr, Deserialize_repr, Clone, PartialEq)]
#[repr(u8)]
pub enum SourceItemKind {
  /// The source item references a normal file.
  File      = 1,

  /// The source item references a directory.
  Directory = 2,
}

impl BspNotification for LogMessageParams {
  const METHOD: &'static str = "build/logMessage";
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct LogMessageParams {
  /// The message type.
  #[serde(rename = "type")]
  pub ty: MessageType,

  /// The task id if any.
  pub task: Option<TaskId>,

  /// The request id that originated this notification.
  /// The originId field helps clients know which request originated a
  /// notification in case several requests are handled by the
  /// client at the same time. It will only be populated if the client
  /// defined it in the request that triggered this notification.
  pub origin_id: Option<String>,

  /// The actual message.
  pub message: String,
}

#[derive(Debug, Default, Serialize_repr, Deserialize_repr, Clone, PartialEq)]
#[repr(u8)]
pub enum MessageType {
  /// An error message.
  Error   = 1,

  /// A warning message.
  Warning = 2,

  /// An information message.
  Info    = 3,

  /// A log message.
  #[default]
  Log     = 4,
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct TaskId {
  /// A unique identifier.
  id: String,

  /// The parent task ids, if any. A non-empty parents field means
  /// this task is a sub-task of every parent task id. The child-parent
  /// relationship of tasks makes it possible to render tasks in
  /// a tree-like user interface or inspect what caused a certain task
  /// execution.
  /// OriginId should not be included in the parents field, there is a separate
  /// field for that.
  parents: Option<Vec<String>>,
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct ExitParams {}

impl BspNotification for ExitParams {
  const METHOD: &'static str = "build/exit";
}
