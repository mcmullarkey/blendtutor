//! # blendtutor-core
//!
//! Domain logic and adapters for blendtutor: the lesson model, language runners
//! (R and Python), grading, and LLM providers — built up slice by slice.
//!
//! This is the reusable core a future Tauri GUI can call directly, with no
//! process boundary. It deliberately does **not** parse command-line arguments
//! or render terminal output; that is the responsibility of the `blendtutor-cli`
//! crate, which depends on this one (the dependency only ever points cli → core).
