use std::path::PathBuf;

use serde::{de::Error, Deserialize, Deserializer};

#[derive(
    Deserialize, Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash,
)]
pub(crate) struct Configuration {
    #[serde(default)]
    pub(crate) fennel: Fennel,
}

#[derive(
    Deserialize, Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash,
)]
pub(crate) struct Fennel {
    #[serde(default)]
    pub(crate) workspace: Workspace,
    #[serde(default)]
    pub(crate) diagnostics: Diagnostics,
}

#[derive(
    Deserialize, Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash,
)]
pub(crate) struct Workspace {
    #[serde(default)]
    pub(crate) library: Vec<Url>,
}

#[derive(
    Deserialize, Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash,
)]
pub(crate) struct Diagnostics {
    #[serde(default)]
    pub(crate) globals: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Url(pub(crate) tower_lsp::lsp_types::Url);

impl<'de> Deserialize<'de> for Url {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s: String = Deserialize::deserialize(deserializer)?;
        tower_lsp::lsp_types::Url::from_directory_path(PathBuf::from(s))
            .map(Self)
            .map_err(|_| D::Error::custom("invalid path"))
    }
}
