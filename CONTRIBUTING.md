# Contributing to netRobust

This document records the branching strategy and merge criteria for this
package -- the working agreement for continuing development, whether
that's future-you, an occasional student collaborator, or an AI coding
session picking the project back up.

## Branch model

Three levels, chosen for individual development with occasional
collaboration:

- **`main`** -- always installable, always passes `devtools::check()`
  with 0 errors/warnings. This is what `devtools::install_github()`
  gives someone by default.
- **`dev`** -- integration branch. Finished features accumulate here
  before a formal release to `main`. Not every feature merge to `dev`
  triggers a `main` release -- that's a deliberate decision by the
  maintainer, made when the accumulated state in `dev` is ready to become
  the new default.
- **`feature/<name>`** -- one branch per extension (e.g.
  `feature/edge-removal`), branched off `dev` (not `main`), so it starts
  from whatever's already accumulated there.

## Merge criteria

### `feature/<name>` → `dev`

All of the following before merging:

1. **Documented** -- roxygen `#'` blocks on every exported function, no
   unresolved cross-reference warnings from `devtools::document()`.
2. **Tested** -- at least one `testthat` test covering the new behaviour,
   and the full suite (`devtools::test()`) passing.
3. **`devtools::check()` clean** -- 0 errors, 0 warnings (0 notes unless
   there's a specific, understood reason for one).
4. **Vignette updated, or explicitly deferred with a note in the PR/commit
   message.** Whether it belongs in the *existing* vignette or warrants a
   *new* one depends on what the feature actually is:
   - **Same narrative, different mechanism** (e.g. edge removal vs. node
     removal, knock-out vs. sequential removal, Shapley-style
     contribution) -- these are all variations on "sequential
     removal + null models + R-index", so they belong as a new section in
     the existing getting-started vignette.
   - **Different analytical framework** (e.g. the planned bridges to
     `NetworkExtinction`, `QPress`, `EMLN` -- cascading extinction,
     qualitative loop analysis, multilayer networks) -- these answer
     different research questions for a potentially different audience.
     Cramming them into the same narrative vignette would make it
     incoherent; they get their own vignette(s) instead.

### `dev` → `main`

A deliberate, maintainer-decided release, not automatic per-feature.
Before merging:

- `devtools::check()` clean on `dev`.
- Consider bumping the version number in `DESCRIPTION` (still
  `0.0.0.9000` as of this writing -- the standard placeholder for
  "in development, never released"). Moving to e.g. `0.1.0` signals "this
  is now a real, usable version" -- a discretionary but conventional
  marker for a first/subsequent release.

## Coding conventions

- **Invalid vocabulary in an argument -> `stop()`. Valid but contradictory
  combinations -> `warning()`.** Never fail silently.
- **`NULL` as the sentinel for "not specified" / "auto-detect"** in
  argument defaults (e.g. `directed = NULL` auto-detects from the graph),
  not `match.arg()`/`identical()` tricks.
- **Explicit namespace qualification (`igraph::`) only where there's an
  actual collision risk** (e.g. `igraph::diversity()` vs.
  `vegan::diversity()`) -- not blanket-qualified everywhere. Confirmed
  against the current codebase: `robustness.R`/`significance.R` qualify
  where needed; the rest rely on `@import igraph` in
  `netRobust-package.R`. Keep new code consistent with this, not with a
  "qualify everything" rule.
- **English** for code, comments, warnings/errors, and documentation --
  the package targets a CRAN release.
- **dplyr/tidyr style** where relevant (see `summarise_null()`), matching
  patterns already established in the codebase.
- **Design before implementing.** Agree on function names, arguments, and
  how a new piece integrates with the existing `keep=`/`exclude=`/
  `optional=` system (or whatever's analogous) before writing code.
