//! Integration tests for the `blendtutor` binary's command-line surface.

use assert_cmd::Command;

/// Subcommands the CLI is planned to expose; each is asserted individually in `--help`.
const PLANNED_SUBCOMMANDS: [&str; 7] = ["init", "new", "validate", "list", "run", "eval", "build"];

#[test]
fn help_lists_subcommands() {
    let output = Command::cargo_bin("blendtutor")
        .expect("binary `blendtutor` should be built")
        .arg("--help")
        .output()
        .expect("running `blendtutor --help` should succeed");

    assert!(
        output.status.success(),
        "`blendtutor --help` should exit 0, got {:?}",
        output.status
    );

    let stdout = String::from_utf8(output.stdout).expect("help output should be UTF-8");
    for subcommand in PLANNED_SUBCOMMANDS {
        // Anchor to the command-list entry (first token of a help line), not a raw
        // substring: `--help`'s about text already contains "run", and "run" is a
        // substring of "running" — both would false-pass a bare `contains` check.
        let listed = stdout
            .lines()
            .any(|line| line.trim_start().split_whitespace().next() == Some(subcommand));
        assert!(
            listed,
            "`--help` should list subcommand `{subcommand}` as a command entry; full output:\n{stdout}"
        );
    }
}
