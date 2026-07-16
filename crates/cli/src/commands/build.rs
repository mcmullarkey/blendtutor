//! `blendtutor build --target <target> <course> -o <out>` — assemble a static,
//! browser-deployable lesson site from a course (ADR-0008).

use std::path::Path;
use std::process::ExitCode;

use anyhow::Context;
use blendtutor_core::course::{Course, SiteConfig};
use blendtutor_core::site::{self, BuildTarget, EmbeddedKey, EvalSummary};

/// The conventional name of the Slice-13 eval report a course bundles to have its
/// accuracy folded into the built site — the JSON `blendtutor eval --format json`
/// emits, dropped next to the course manifest.
const EVAL_REPORT_FILE: &str = "eval-report.json";

/// Parse the `--target` value into a [`BuildTarget`].
///
/// A custom parser rather than deriving `clap::ValueEnum` on the core type, so
/// `core` stays free of the `clap` dependency — the dependency only ever points
/// cli → core (mirrors `new`'s `parse_language`).
pub fn parse_target(value: &str) -> Result<BuildTarget, String> {
    match value {
        "webr" => Ok(BuildTarget::Webr),
        "pyodide" => Ok(BuildTarget::Pyodide),
        other => Err(format!(
            "unknown build target {other:?}; expected `webr` or `pyodide`"
        )),
    }
}

/// Open the course, load its lessons, fold in any eval report, plan the site,
/// optionally encrypt it with a password, and write it to `out`.
///
/// Gather → transform (pure) → persist: opening the course, reading the eval
/// report, and writing the site are the effectful shell around the pure
/// [`site::plan_site`]. A language/target mismatch is refused by `plan_site`
/// *before* `write_site` runs (the `?` short-circuits), so a refused build never
/// creates `out` (§1.3.1). When `password` is `Some`, the planned site is
/// post-processed by [`site::encrypt_site_files`] before writing — a separate
/// pure step that preserves `plan_site`'s contract (§2.1).
///
/// When `embed_key` is `Some`, it must be accompanied by `password` (the key
/// is encrypted into the payload — without a password there is nothing to
/// encrypt with). The key is validated against the provider's expected prefix
/// (fireworks → `fw_`, anthropic → `sk-ant-`) and a CLI warning is emitted to
/// stderr about the key being unique/single-use and the need to set a spend
/// limit. Both validations run *before* `plan_site`, so a refused build never
/// creates `out` (§1.3.1).
pub fn run(
    dir: &Path,
    target: BuildTarget,
    out: &Path,
    password: Option<&str>,
    embed_key: Option<&str>,
) -> anyhow::Result<ExitCode> {
    // Parse and validate the embed key BEFORE any work — a refusal here means
    // no output directory is created (§1.3.1). The key requires a password
    // (it is encrypted into the payload), and the provider/key prefix must
    // match (fireworks → fw_, anthropic → sk-ant-).
    let embedded_key = parse_embed_key(embed_key, password)?;

    let course = Course::open(dir)?;
    let lessons = course.load_lessons()?;
    let eval = load_eval_summary(dir)?;
    // The site config from the manifest's [site] section, or the default
    // (max_feedback_per_session = 20) when absent — the caller decides the
    // default, not plan_site (§2.1: plan_site is pure, takes explicit input).
    let default_site = SiteConfig::default();
    let site_config = course.site_config().unwrap_or(&default_site);
    let planned = site::plan_site(&lessons, target, &eval, site_config)?;
    let site = if let Some(pw) = password {
        let mut rng = rand_core::OsRng;
        site::encrypt_site_files(&planned, pw, embedded_key.as_ref(), &mut rng)
    } else {
        planned
    };
    site::write_site(out, &site)?;
    if password.is_some() {
        println!(
            "built {} lesson(s) for {target} into {} (password-protected)",
            lessons.len(),
            out.display()
        );
    } else {
        println!(
            "built {} lesson(s) for {target} into {}",
            lessons.len(),
            out.display()
        );
    }
    Ok(ExitCode::SUCCESS)
}

/// Read the course's bundled eval report, if any, into an [`EvalSummary`].
///
/// The effectful edge of the pure fold (§2.3): an *absent* `eval-report.json` is
/// the represented [`EvalSummary::NotValidated`] state (§1.2) — not an error — so a
/// course that simply hasn't been evaluated still builds. A *present* report is
/// parsed as-is (§3.2); a present-but-unreadable one fails the build, so a corrupt
/// report never silently unvalidates a course.
fn load_eval_summary(dir: &Path) -> anyhow::Result<EvalSummary> {
    let path = dir.join(EVAL_REPORT_FILE);
    match std::fs::read_to_string(&path) {
        Ok(json) => Ok(site::eval_summary_from_report_json(&json)?),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(EvalSummary::NotValidated),
        Err(e) => Err(e).with_context(|| format!("reading {}", path.display())),
    }
}

/// The expected key prefix for each provider — used to validate that the
/// provider and key are consistent (a fireworks key starts `fw_`, an Anthropic
/// key starts `sk-ant-`). A mismatch is refused at the CLI boundary (§1.3.1)
/// so a key encrypted under the wrong provider never ships.
const FIREWORKS_KEY_PREFIX: &str = "fw_";
const ANTHROPIC_KEY_PREFIX: &str = "sk-ant-";

/// Parse and validate the `--embed-key` value (`provider:key`), returning an
/// [`EmbeddedKey`] when valid.
///
/// Validations (all run before `plan_site`, so a refusal creates no output):
/// - `--embed-key` requires `--password` (the key is encrypted into the
///   payload — without a password there is nothing to encrypt with).
/// - The provider must be `fireworks` or `anthropic`.
/// - The key prefix must match the provider (`fw_` for fireworks, `sk-ant-`
///   for anthropic).
///
/// On success, emits a CLI warning to stderr about the key being unique /
/// single-use and the need to set a spend limit / budget to bound cost.
fn parse_embed_key(
    embed_key: Option<&str>,
    password: Option<&str>,
) -> anyhow::Result<Option<EmbeddedKey>> {
    let Some(raw) = embed_key else {
        return Ok(None);
    };
    // --embed-key requires --password — the key is encrypted into the payload.
    let pw = password.ok_or_else(|| {
        anyhow::anyhow!(
            "--embed-key requires --password (the API key is encrypted into the site payload; \
             without a password there is nothing to encrypt with)"
        )
    })?;
    // Format: provider:key — split on the FIRST colon (the key itself may
    // contain colons in some providers, though fireworks/anthropic keys do not).
    let (provider, key) = raw.split_once(':').ok_or_else(|| {
        anyhow::anyhow!(
            "--embed-key format is `provider:key` (e.g. `fireworks:fw_xxx`, \
             `anthropic:sk-ant-xxx`)"
        )
    })?;
    // Validate the provider is a known one.
    let expected_prefix = match provider {
        "fireworks" => FIREWORKS_KEY_PREFIX,
        "anthropic" => ANTHROPIC_KEY_PREFIX,
        other => {
            return Err(anyhow::anyhow!(
                "unknown provider {other:?}; expected `fireworks` or `anthropic`"
            ));
        }
    };
    // Validate the key prefix matches the provider.
    if !key.starts_with(expected_prefix) {
        return Err(anyhow::anyhow!(
            "key for provider `{provider}` must start with `{expected_prefix}` \
             (got a key starting with `{}`) — the provider and key must be consistent",
            &key[..key.len().min(expected_prefix.len() + 4)]
        ));
    }
    // Emit a CLI warning about the embedded key. The key is unique to this
    // site and should be single-use with a spend limit to bound cost.
    eprintln!(
        "WARNING: The embedded API key for {provider} is unique to this site and should be \
         single-use. Set a spend limit / budget on the key to bound cost. Anyone with the \
         site password can extract the key — use a dedicated key with a tight spend limit."
    );
    // Suppress unused-variable warning for pw (it is validated but not used
    // beyond the presence check — the password flows through encrypt_site_files
    // separately).
    let _ = pw;
    Ok(Some(EmbeddedKey {
        provider: provider.to_string(),
        key: key.to_string(),
    }))
}
