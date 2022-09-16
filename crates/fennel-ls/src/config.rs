use serde::Deserialize;

#[derive(
    Deserialize, Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash,
)]
pub(crate) struct Configuration {
    pub(crate) fennel: Fennel,
}

#[derive(
    Deserialize, Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash,
)]
pub(crate) struct Fennel {
    pub(crate) diagnostics: Diagnostics,
}

#[derive(
    Deserialize, Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash,
)]
pub(crate) struct Diagnostics {
    pub(crate) globals: Vec<String>,
}
