# Evidence — issue #230: `eval --write-report`

Captured after the final code state (commits 5041599 → 0592438 → dc8a170).

- `test-suite.log` — `cargo test -p blendtutor-cli --test eval`: 16 passed
  (9 pre-existing unchanged + 7 new AC-8 tests: positive items 1-6 incl. the
  `--format json` byte-identity pin, N1 no-course-root refusal before scoring,
  N2 overwrite warns-and-proceeds, N3 partial refused, N4 write failure
  propagates with no `.tmp` leftover, build round-trip item 8, `eval --help`
  advertises the flag item 7).
- `cli-transcript.log` — real-binary E2E transcript against a local mock
  provider (OpenAI chat-completions envelope on 127.0.0.1:8765): `eval --help`
  grep; positive run from a non-course-root CWD (exit 0, human render
  unchanged, `eval-report.json` at course root and NOT at CWD, full shape
  `{cases: 3, accuracy: 2/3}`, confirmation line); overwrite second run
  (WARNING + exit 0); `build --target webr` round-trip (`67%` in
  `eval-results.html`); N1 refusal (exit 1, names `blendtutor.toml`, no file);
  N3 refusal (exit 1, names both flags).

The positive path in the transcript is the same wire shape the wiremock tests
pin (`crates/cli/tests/common/mod.rs` `feedback_body`), so transcript and
suite corroborate each other.
