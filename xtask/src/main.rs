//! See <https://github.com/matklad/cargo-xtask/>.
//!
//! This binary defines various auxiliary build commands, which are not
//! expressible with just `cargo`. Notably, it provides tests via `cargo test -p xtask`
//! for code generation and `cargo xtask install` for installation of
//! rust-analyzer server and client.
//!
//! This binary is integrated into the `cargo` command line by using an alias in
//! `.cargo/config`.

#![warn(rust_2018_idioms, unused_lifetimes)]
#![allow(
    clippy::print_stderr,
    clippy::print_stdout,
    clippy::disallowed_methods,
    clippy::disallowed_types
)]

mod flags;

mod codegen;

use anyhow::bail;
use std::{env, path::PathBuf};
use xshell::{cmd, Shell};

fn main() -> anyhow::Result<()> {
    let flags = flags::Xtask::from_env_or_exit();

    let sh = &Shell::new()?;
    sh.change_dir(project_root());

    match flags.subcommand {
        flags::XtaskCmd::Codegen(cmd) => cmd.run(sh),
    }
}

/// Returns the path to the root directory of `rust-analyzer` project.
fn project_root() -> PathBuf {
    let dir =
        env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned());
    PathBuf::from(dir).parent().unwrap().to_owned()
}
