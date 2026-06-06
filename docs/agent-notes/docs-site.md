---
topic: docs-site
created: 2026-06-06
slices: [3]
---

How the combined documentation site (mdBook narrative + rustdoc API → GitHub
Pages) is built, merged, and deployed (Slice 3).

- 2026-06-06 (#3): The site is **two tools stitched together**, not one. mdBook
  owns the prose (`docs/book/`), rustdoc owns the API surface (`cargo doc`). At
  deploy time the rustdoc tree is copied to `docs/book/book/api`, so one artifact
  serves the book at `/` and the API at `/api/blendtutor_core/`. There is no
  Material/MkDocs-style tool that subsumes both in Rust — don't go looking for one.
- 2026-06-06 (#3): **mdBook 0.5 content-hashes static asset filenames.** Search
  ships as `searchindex-<hash>.js` / `searcher-<hash>.js`, not the bare
  `searchindex.js` / `searcher.js` that 0.4 (and the slice spec) assumed. Any
  check that asserts search output must glob (`searchindex*.js`), not match a
  fixed name. `scripts/check-docs.sh` does this; the bare-name predicate can
  never pass on 0.5.x even though search is enabled and working.
- 2026-06-06 (#3): **AC "no rustdoc warnings" is enforced two ways, and you need
  both.** `RUSTDOCFLAGS="-D warnings"` catches default-warn lints (e.g. broken
  intra-doc links) but `missing_docs` is allow-by-default — an undocumented `pub`
  item would *not* warn. `crates/core/src/lib.rs` declares `#![deny(missing_docs)]`
  to close that gap, so a missing doc fails ordinary `cargo build`/clippy too, not
  only the docs build.
- 2026-06-06 (#3): `scripts/check-docs.sh` is the local executable spec — it runs
  the same rustdoc + mdBook build, assembles the merged dir, and asserts the
  predicates. `docs.yml` keeps the build commands **inline** (rather than calling
  the script) so the deploy pipeline reads standalone and is greppable; the small
  command overlap is deliberate. Run the script by hand to reproduce CI locally.
- 2026-06-06 (#3): `docs.yml` triggers on push to the **default branch (`main`)**
  plus `workflow_dispatch`. During the Rust rewrite the code lives on
  `staging/rewrite-in-rust-lol`, so the Pages deploy does not fire until staging
  merges to main; use `workflow_dispatch` to exercise it earlier. Pages needs a
  one-time repo setting (Settings → Pages → Source: GitHub Actions) that the
  workflow cannot self-enable.
- 2026-06-06 (#3): Pages deploy hygiene — least-privilege `permissions` scoped
  **per job**: the workflow default is `contents: read` (checkout), and the
  `pages: write` + `id-token: write` OIDC grants live on the `deploy` job only,
  not the build job (job-level `permissions` replaces the workflow default). A
  `concurrency: { group: pages, cancel-in-progress: false }` stops a cancelled
  run leaving the live site half-published. The rendered output
  (`/docs/book/book/`) is gitignored; only `docs/book/{book.toml,src/}` is tracked.
