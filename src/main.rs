//! Substrate Node Template CLI library.

#![warn(missing_docs)]
#![warn(unused_extern_crates)]

mod chain_spec;
mod cli;
#[macro_use]
mod service;

pub use substrate_cli::{error, IntoExit, VersionInfo};

fn main() {
	let version = VersionInfo {
		name: "Substrate Node",
		commit: env!("VERGEN_SHA_SHORT"),
		version: env!("CARGO_PKG_VERSION"),
		executable_name: "kusama-kitties",
		author: "bryan.chen",
		description: "kusama-kitties",
		support_url: "support.anonymous.an",
	};

	if let Err(e) = cli::run(::std::env::args(), cli::Exit, version) {
		eprintln!("Fatal error: {}\n\n{:?}", e, e);
		std::process::exit(1)
	}
}
