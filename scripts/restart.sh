#!/usr/bin/env bash

set -e

BUILD_DUMMY_WASM_BINARY= cargo check
WASM_BUILD_TYPE=release cargo run -- purge-chain --dev -y
WASM_BUILD_TYPE=release cargo run -- --dev
