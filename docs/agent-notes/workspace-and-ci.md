---
topic: workspace-and-ci
created: 2026-06-06
slices: [1]
---

How the Rust workspace, tests, and CI fit together (Slice 1 walking skeleton).

- 2026-06-06 (#1): Two-crate workspace — `blendtutor-core` (lib, domain logic +
  adapters) and `blendtutor-cli` (bin `blendtutor`, thin shell). The dependency
  only ever points cli → core; core never imports cli (ADR-0001). `core` is
  `anyhow`-free — `anyhow` lives only at the CLI edge.
- 2026-06-06 (#1): The command set is a clap `Commands` sum type in
  `crates/cli/src/main.rs`. To add a command: add a variant, add its arm to
  `Commands::name()` (exhaustive — the compiler forces it), and dispatch it to a
  `core` entry point. Until a command's slice lands it returns
  `blendtutor_core::NotYetImplemented`.
- 2026-06-06 (#1): Binary name is `blendtutor` (`[[bin]] name`), and clap's
  version line is pinned to `blendtutor {version}` via
  `#[command(name = "blendtutor", version)]`. CLI tests assert against
  `env!("CARGO_PKG_VERSION")`, never a hardcoded version literal.
- 2026-06-06 (#1): Tests run with `cargo nextest run`. `.config/nextest.toml`'s
  default profile excludes tests whose name contains `live` (real external
  services); run those with `cargo nextest run --profile live` (used from
  Slice 10). Real-interpreter/integration tests should skip-with-notice when the
  tool is absent rather than fail spuriously.
- 2026-06-06 (#1): Local pre-push gate mirrors CI exactly —
  `cargo nextest run --locked && cargo clippy --all-targets --locked -- -D warnings && cargo fmt --all --check`.
- 2026-06-06 (#1): Toolchain is pinned in `rust-toolchain.toml` (1.94.0 +
  rustfmt, clippy) as the single source of truth; CI lets rustup auto-install
  from it instead of pinning a second time in the workflow.
