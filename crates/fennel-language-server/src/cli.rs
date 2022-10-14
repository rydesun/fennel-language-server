use clap::{Parser, Subcommand};

#[derive(Parser, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[command(name = env!("CARGO_PKG_NAME"))]
#[command(bin_name = env!("CARGO_PKG_NAME"))]
#[command(author, version, about, long_about = None)]
pub(crate) struct Cli {
    #[command(subcommand)]
    pub(crate) cmd: Option<Command>,
}

#[derive(Subcommand, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Command {
    /// Run the language server.
    Lsp {
        #[clap(subcommand)]
        cmd: LspCommand,
    },
}

#[derive(Subcommand, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum LspCommand {
    /// Listen on a TCP address.
    Tcp {
        /// The address and port to listen on.
        #[clap(long)]
        address: String,
    },
    /// Attach to standard input and output.
    Stdio,
}
