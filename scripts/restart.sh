#!/usr/bin/env bash

set -e

cargo run -- purge-chain --dev -y
cargo run -- --dev
