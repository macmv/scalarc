#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Scala {}

pub type SyntaxNode = rowan::SyntaxNode<Scala>;
pub type SyntaxToken = rowan::SyntaxToken<Scala>;
pub type SyntaxElement = rowan::SyntaxElement<Scala>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<Scala>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<Scala>;
pub type PreorderWithTokens = rowan::api::PreorderWithTokens<Scala>;
