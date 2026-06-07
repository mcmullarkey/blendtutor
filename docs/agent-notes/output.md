---
topic: output
created: 2026-06-06
slices: [5]
---

How command results reach the terminal (`cli::output`). The renderer seam every
later command renders through. See [[lesson-model]] for the typed model that
feeds it.

- 2026-06-06 (#5): `cli::output` is the **renderer seam** (§3.2, §3.4). A command
  computes a typed result value and hands it to `output`; nothing in command
  logic knows about formats or terminals. `validate` returns a `ValidateReport`
  and calls `output::emit_validate(&report, format)`. Adding a format is a new
  `OutputFormat` variant + arms in `render_validate` — zero command changes.
  Every later command follows this shape: compute a typed result, render it here.
- 2026-06-06 (#5): The **exit code is read from the result**
  (`ValidateReport::exit_code`), not the renderer — computed once, format-
  independent. This is what makes `--format json` and `--format human` exit
  identically on the same input (#5 AC1). A command returning a result + an
  exit code keeps "what happened" separate from "how it's shown".
- 2026-06-06 (#5): **stdout vs stderr is a rendering decision, kept in `output`.**
  JSON always goes to stdout (it is the machine document, piped to `jq`). Human
  output sends the success line to stdout (data) and validation findings to
  stderr (diagnostics). `render_validate` returns `Rendered { text, stream }`
  (pure, snapshot-testable); `emit_validate` is the single effect that writes.
  The stderr-for-findings choice is also what keeps slice-4's CLI tests green
  (they assert the problem text on stderr).
- 2026-06-06 (#5): A **read (I/O) error stays an `anyhow` error** propagated to
  `main` via `?`; only `LoadError::Invalid` becomes a rendered finding. A missing
  file is not a validation finding — this preserves the slice-4 invariant that
  `LoadError::Read` and `LoadError::Invalid` stay distinct ([[lesson-model]]).
  Consequence: `main` returns `anyhow::Result<ExitCode>` (Ok carries the report's
  code, Err is a genuine failure printed to stderr).
- 2026-06-06 (#5): `OutputFormat` is a clap `ValueEnum` (§1.2) with a hand-written
  `Display` that defers to `self.to_possible_value().get_name()`, so the
  `--format` default (`default_value_t = OutputFormat::Human`) can never drift
  from the spelling clap parses. Rename a variant and the default follows.
- 2026-06-06 (#5): JSON is serialized through a **separate `ValidateDocument`
  view** (`{ status, lesson_name?, findings }`), not by deriving `Serialize` on
  the domain type. The wire shape is decoupled from the internal representation,
  and `findings` is always present (`[]` when valid) so consumers can rely on the
  key. `status` is a string discriminant (`"valid"`/`"invalid"`).
- 2026-06-06 (#5): An invalid outcome carries **≥1 finding by construction** —
  `Findings(Vec<String>)` whose only constructor `new(first, rest)` requires a
  leading element (§1.1, roborev gate). An "invalid lesson with no stated
  problem" is unrepresentable, not merely unreached.
- 2026-06-06 (#5): Human format is pinned by an **`insta` snapshot** over
  `render_validate(report, Human)` (unit test in `output.rs`). `*.snap.new` is
  gitignored; review + accept turns a pending snapshot into the committed
  `.snap`. `cargo-insta` is not installed in the dev image — bless via
  `INSTA_UPDATE=always cargo nextest run -E 'test(<name>)'` instead.
