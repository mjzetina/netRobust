#### SIGNIFICANCE ####
# Manuel J. Zetina-Rejon
# Statistical significance framework for robustness curves
#
# Idea: a single random_failures() run is one draw from the space of all
# possible removal orders. To know whether a targeted attack curve is
# "significantly" more damaging than chance, we build a null distribution
# from many random-removal replicates and compare the observed (attack)
# curve against it, step by step.
#
# Two null models are available, answering different questions:
#   - robustness_null_removal(): fixes the network, randomises the removal
#     order. "Is this attack strategy more damaging than random failure, on
#     this specific network?" -- the Albert et al. (2000) framing.
#   - robustness_null_structural(): fixes the removal *strategy*, randomises
#     the network's topology (degree-preserving rewiring). "Is this
#     network's structure itself more/less robust to this attack strategy
#     than expected by chance, given its size and degree sequence?"
#
# Requires: igraph (already used elsewhere). Uses future.apply for
# parallel replicates if installed; falls back to plain lapply otherwise.
#   install.packages("future.apply")
# Reports a console progress indicator (spinner + percentage) while
# replicates run sequentially -- see .console_progress() below.

# --- 0. Progress reporting (internal) ---------------------------------------
# A plain console spinner + percentage, no setup required. Only meaningful
# for sequential replicates: a future.apply worker is a separate R process
# with no access to the parent session's console, so this is only ever
# called from the sequential branch (see use_parallel below), and only
# prints in an interactive session -- never during R CMD check, a sourced
# script, or a vignette/document build, where a \r-updating line would just
# pollute the log.
.spinner_chars <- c("|", "/", "-", "\\")

.console_progress <- function(i, n) {
  if (!interactive()) return(invisible(NULL))
  ch  <- .spinner_chars[(i - 1L) %% length(.spinner_chars) + 1L]
  pct <- round(100 * i / n)
  cat(sprintf("\r%s %3d%% (%d/%d)", ch, pct, i, n))
  if (i >= n) cat("\n")
  utils::flush.console()
  invisible(NULL)
}

# Whether replicates should actually run through future.apply -- TRUE only
# when `parallel = TRUE`, the future.apply package is installed, AND a real
# (non-sequential) future::plan() with more than one worker is active. Package
# availability alone used to be the only check, which silently misfired: under
# future's default *sequential* plan, future_lapply() runs in the very same
# process plain lapply() would -- so gating the console spinner on "is
# future.apply installed" (reasoned as "a worker has no console access")
# suppressed it even when there was no separate worker at all, leaving
# long-running replicates with no visible feedback and no actual speed-up.
.use_parallel <- function(parallel) {
  parallel && requireNamespace("future.apply", quietly = TRUE) &&
    future::nbrOfWorkers() > 1
}

# --- 1. Build the null distribution (removal-order randomisation) ----------
#' Monte Carlo null distribution of robustness curves (random removal order)
#'
#' Runs `n_reps` independent random-failure simulations of the same network
#' (via [random_failures()] + [robustness()]), producing the empirical null
#' distribution of every robustness indicator at every removal step. This is
#' the statistical baseline against which a targeted attack curve can be
#' judged: if a single random draw is unusual to expect, thousands of them
#' trace out the range of outcomes chance alone would produce.
#'
#' The network itself is fixed here -- only the removal order is randomised.
#' To instead ask whether the network's *structure* is unusually robust
#' relative to random networks with the same degree sequence, see
#' [robustness_null_structural()].
#'
#' @section Performance on large networks:
#' Every replicate recomputes every indicator at every removal step, so
#' runtime scales with `n_reps * vcount(g) * (indicator cost)` -- for
#' `diameter`/`mean_distance` specifically, each step costs roughly
#' `O(V * (V + E))` (all-pairs shortest paths). On a network with thousands
#' of edges, the default `n_reps = 500` with the default indicator set can
#' take a long time -- this isn't specific to netRobust, it's inherent to
#' recomputing every indicator at every step, for every replicate, the same
#' cost any implementation of this Albert-Jeong-Barabasi/Schneider et al.
#' style simulation faces. Three practical levers, roughly in order of
#' effort:
#' \enumerate{
#'   \item **Parallelise**: `future::plan(future::multisession)` before
#'     calling -- see `parallel=` above.
#'   \item **Explore with fewer replicates, finalise with more**: e.g. start
#'     with `n_reps = 50`-`100` while iterating on the analysis, and only
#'     run the full `n_reps` (or more) once, for the final result -- the
#'     standard Monte Carlo workflow, and why the default here is a modest
#'     500 rather than the 10,000 used for final results (see `n_reps=`
#'     above).
#'   \item **Restrict `keep=` to cheap indicators**: `giant_component_size`,
#'     `connectivity`, `clustering`, `edge_density`, and
#'     `degree_centralization` are comparatively cheap; `diameter` and
#'     `mean_distance` are the most expensive indicators in the default set.
#' }
#' A qualitatively faster approach exists for `giant_component_size`/
#' `connectivity` specifically -- Newman & Ziff's (2001)
#' <doi:10.1103/PhysRevE.64.016706> reverse-construction, union-find-based
#' algorithm for percolation simulation -- but it doesn't extend to
#' `diameter`/`mean_distance`, and isn't implemented here.
#'
#' @param g An `igraph` object.
#' @param n_reps Number of random-failure replicates. Rubio's thesis (this
#'   project's methodological source for this framework) uses 10,000 for
#'   final results; lower values (the default 500) are fine for quick
#'   exploration.
#' @param parallel If `TRUE` (default), the `future.apply` package is
#'   installed, AND a non-sequential backend has actually been set with
#'   `future::plan()` (e.g. `future::plan(future::multisession)`), replicates
#'   run in parallel across that backend. Installing `future.apply` alone
#'   isn't enough -- without an explicit `future::plan()` call, replicates
#'   run sequentially via plain `lapply()` regardless of this argument, with
#'   the console progress indicator shown (see `.console_progress()`'s
#'   notes): a `future.apply` worker only lacks console access once a real,
#'   separate worker process exists.
#' @param seed Optional integer passed to `set.seed()` for reproducibility.
#' @param ... Forwarded as-is to every internal [robustness()] call (e.g.
#'   `indicators=`, `exclude=`, `keep=`, `custom=`, `directed=`,
#'   `component_mode=`, `mode=`, `weights=`, `weights_as=`). This matters:
#'   for the null distribution to be a valid statistical baseline for an
#'   observed attack/failure curve, it must be built with the *exact same*
#'   settings as that curve -- same indicators, same directed/weights
#'   treatment. Pass here whatever extra arguments you used when computing
#'   the scenario you're testing against, e.g.
#'   `robustness_null_removal(g, keep = c("giant_component_size", "mean_distance"), directed = TRUE)` to
#'   match `robustness(g, ids, keep = c("giant_component_size", "mean_distance"), directed = TRUE)`.
#'
#' @return A `data.frame` stacking all replicates: same columns as
#'   [robustness()]'s output, plus `rep` identifying which replicate each
#'   row came from. Pass this to [summarise_null()], [compare_to_null()], or
#'   [robustness_index_null()].
#'
#' @seealso [robustness()], [random_failures()], [robustness_null_structural()],
#'   [summarise_null()], [compare_to_null()], [robustness_index_null()]
#' @export
robustness_null_removal <- function(g, n_reps = 500, parallel = TRUE, seed = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)
  use_parallel <- .use_parallel(parallel)

  run_one <- function(i) {
    ids <- random_failures(g)
    res <- robustness(g, ids, ...)
    res$rep <- i
    if (!use_parallel) .console_progress(i, n_reps)
    res
  }

  reps <- if (use_parallel) {
    future.apply::future_lapply(seq_len(n_reps), run_one, future.seed = TRUE)
  } else {
    lapply(seq_len(n_reps), run_one)
  }

  do.call(rbind, reps)
}
# To run replicates in parallel across cores:
#   future::plan(future::multisession)   # once, at the top of main.R
# (the console progress indicator only shows for sequential replicates --
# see .console_progress() above for why)

# --- 1b. Build the null distribution (structural randomisation) ------------
#' Monte Carlo null distribution of robustness curves (structural randomisation)
#'
#' Runs `n_reps` independent replicates of a *topological* null model: each
#' replicate rewires `g` via `igraph::rewire()` with
#' `igraph::keeping_degseq()` (preserving the degree sequence, but
#' randomising which vertices connect to which), applies `attack_fn` to that
#' randomised graph to get a removal order, and runs [robustness()] on it.
#'
#' This answers a different question from [robustness_null_removal()]: not
#' "does this attack strategy do more damage than chance, on this network?"
#' but "is this network's structure itself more/less robust to this attack
#' strategy than expected by chance, given its size and degree sequence?"
#'
#' Pure topological null: edge weights are dropped before rewiring (random
#' rewiring has no principled way to reassign the observed weights to new
#' edges -- edge weights in ecological/food-web data in particular tend to
#' carry a lot of uncertainty, so this is the conservative choice for now).
#' Every replicate `attack_fn` and `...` see is therefore an *unweighted*
#' graph. Pass a weight-agnostic `attack_fn` (e.g. `attack_by_degree`, not a
#' closure that reads `E(g)$weight`), and don't rely on `weights_as=` in
#' `...` having any effect here.
#'
#' @param g An `igraph` object -- the observed network whose size and degree
#'   sequence are preserved across replicates.
#' @param attack_fn A `function(g)` returning a removal order (the same
#'   contract as [random_failures()] and every `attack_by_*()` function),
#'   applied fresh to each rewired replicate -- e.g. `attack_by_degree`, or a
#'   wrapper like `function(g) attack_by_degree(g, mode = "in")`.
#' @param n_reps Number of rewired replicates.
#' @param rewire_niter Number of double-edge-swap trials passed to
#'   `igraph::keeping_degseq(niter = )`. `NULL` (default) uses `10 *
#'   ecount(g)`, a common rule of thumb for adequately randomising a graph.
#' @param parallel,seed As in [robustness_null_removal()] -- see its
#'   Performance section for guidance on large networks (same cost profile
#'   here: one full curve per replicate).
#' @param ... Forwarded to every internal [robustness()] call -- must match
#'   the settings used for the observed curve you're comparing against (see
#'   [robustness_null_removal()] for why), minus any weight-related settings
#'   (see Details).
#'
#' @return Same shape as [robustness_null_removal()]: a `data.frame` with one
#'   column per indicator, `n_removed`, and `rep`.
#'
#' @seealso [robustness_null_removal()], [summarise_null()],
#'   [robustness_index_null()]
#' @export
robustness_null_structural <- function(g, attack_fn, n_reps = 500, rewire_niter = NULL,
                                        parallel = TRUE, seed = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)
  if (is.null(rewire_niter)) rewire_niter <- 10 * ecount(g)
  use_parallel <- .use_parallel(parallel)

  # Weights are stripped by design (see Details) -- robustness()'s own
  # "weights_as has no effect on an unweighted graph" warning would otherwise
  # fire once per replicate, which is just noise once you already know why
  # (documented above). Muffle only that exact warning; anything else
  # robustness() raises (e.g. an indicator genuinely failing) still surfaces.
  run_one <- function(i) {
    gi <- rewire(g, with = keeping_degseq(niter = rewire_niter))
    if ("weight" %in% edge_attr_names(gi)) gi <- delete_edge_attr(gi, "weight")
    ids <- attack_fn(gi)
    res <- withCallingHandlers(
      robustness(gi, ids, ...),
      warning = function(w) {
        if (grepl("has no effect: g is an unweighted graph", conditionMessage(w), fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
    res$rep <- i
    if (!use_parallel) .console_progress(i, n_reps)
    res
  }

  reps <- if (use_parallel) {
    future.apply::future_lapply(seq_len(n_reps), run_one, future.seed = TRUE)
  } else {
    lapply(seq_len(n_reps), run_one)
  }

  do.call(rbind, reps)
}

# --- 1c. Build the null distribution (edge removal-order randomisation) ----
#' Monte Carlo null distribution of edge-robustness curves (random removal order)
#'
#' The edge-level counterpart of [robustness_null_removal()]: runs `n_reps`
#' independent random-edge-failure simulations of the same network (via
#' [random_edge_failures()] + [edge_robustness()]), producing the empirical
#' null distribution of every robustness indicator at every edge-removal
#' step. Statistical baseline for an edge-based attack curve (from
#' [attack_by_edge_betweenness()]/[attack_by_edge_weight()]), the same way
#' [robustness_null_removal()] is for a node-based one.
#'
#' `g`'s parallel edges (if any) are resolved *once*, up front -- not once
#' per replicate. Unlike composing [random_edge_failures()] and
#' [edge_robustness()] by hand yourself (where each call independently
#' checks and, if applicable, re-warns about the same parallel edges),
#' this warns at most once for the whole run, since `g`'s parallel-edge
#' status doesn't change across replicates.
#'
#' @param g An `igraph` object.
#' @param n_reps Number of random-edge-failure replicates.
#' @param on_parallel How to handle parallel edges (multiple edges between
#'   the same pair of vertices), resolved once before any replicates run.
#'   `"collapse"` (default) merges them via `igraph::simplify()`, summing
#'   weights if any, with a warning. `"error"` stops instead.
#' @param parallel,seed,... As in [robustness_null_removal()] -- `...` is
#'   forwarded to every internal [edge_robustness()] call, and must match
#'   whatever settings were used for the observed edge-attack curve you're
#'   testing against. See [robustness_null_removal()]'s Performance section
#'   for guidance on large networks (same cost profile here: one full curve
#'   per replicate, over `ecount(g)` steps instead of `vcount(g)`).
#'
#' @return A `data.frame` stacking all replicates: same columns as
#'   [edge_robustness()]'s output, plus `rep` identifying which replicate
#'   each row came from. Pass this to [summarise_null()], [compare_to_null()],
#'   or [robustness_index_null()] exactly like a node-removal null.
#'
#' @seealso [edge_robustness()], [random_edge_failures()],
#'   [robustness_null_removal()], [summarise_null()], [compare_to_null()]
#' @export
edge_robustness_null_removal <- function(g, n_reps = 500, on_parallel = c("collapse", "error"),
                                          parallel = TRUE, seed = NULL, ...) {
  on_parallel <- match.arg(on_parallel)
  # Resolve parallel edges ONCE, up front -- g's parallel-edge status doesn't
  # change across replicates, so collapsing (and warning, if applicable)
  # here means every per-replicate call below sees an already-clean graph
  # and its own internal .prepare_edge_graph() check becomes a no-op,
  # instead of re-warning about the same thing on every one of n_reps
  # replicates (random_edge_failures() AND edge_robustness() would each
  # otherwise re-check it separately, doubling that noise again).
  g <- .prepare_edge_graph(g, on_parallel = on_parallel)
  if (!is.null(seed)) set.seed(seed)
  use_parallel <- .use_parallel(parallel)

  run_one <- function(i) {
    ids <- random_edge_failures(g)
    res <- edge_robustness(g, ids, ...)
    res$rep <- i
    if (!use_parallel) .console_progress(i, n_reps)
    res
  }

  reps <- if (use_parallel) {
    future.apply::future_lapply(seq_len(n_reps), run_one, future.seed = TRUE)
  } else {
    lapply(seq_len(n_reps), run_one)
  }

  do.call(rbind, reps)
}

# --- 1d. Build the null distribution (random-group knockout) ---------------
#' Monte Carlo null distribution of knock-out results (random groups)
#'
#' The [knockout_robustness()] counterpart of [robustness_null_removal()]:
#' generates `n_reps` *random* groups of each size in `sizes` and knocks
#' each one out independently (via [knockout_robustness()]), producing an
#' empirical null distribution stratified by group size. Answers "is
#' removing *this particular* group unusual, compared to removing a random
#' group of the same size?" -- see [compare_knockout_to_null()] and
#' [plot_knockout_null()] to actually make that comparison.
#'
#' Stratified by size because [knockout_robustness()] output isn't a
#' trajectory the way [robustness()]'s is -- an indicator's value for a
#' 1-vertex knock-out and a 5-vertex knock-out aren't comparable the way
#' two points on the same curve are, so a single pooled null wouldn't mean
#' anything. [compare_knockout_to_null()] always compares an observed
#' group against *only* the null replicates of matching size.
#'
#' @param g An `igraph` object.
#' @param sizes Integer vector of group sizes to build a null for -- must
#'   cover every group size you intend to test with
#'   [compare_knockout_to_null()]/[plot_knockout_null()] afterwards.
#' @param n_reps Number of random groups to generate *per size* (so the
#'   total number of trials is `n_reps * length(sizes)`).
#' @param seed Optional integer passed to `set.seed()` for reproducibility.
#' @param ... Forwarded to the internal [knockout_robustness()] call --
#'   must match whatever settings were used for the observed knock-out(s)
#'   you're testing against. Don't pass `include_intact=` here -- this
#'   function always calls [knockout_robustness()] with `include_intact =
#'   FALSE` (a reference row has no place mixed into a null distribution's
#'   replicates), so supplying it too would error on a duplicate argument.
#'
#' @return A `data.frame`, same shape as [knockout_robustness()]'s output
#'   with `include_intact = FALSE` (`group_id`, `group`, `n_removed`, one
#'   column per indicator) -- one row per random trial, `n_removed`
#'   identifying which size stratum it belongs to.
#'
#' @seealso [knockout_robustness()], [compare_knockout_to_null()],
#'   [plot_knockout_null()], [robustness_null_removal()]
#' @export
knockout_robustness_null_removal <- function(g, sizes, n_reps = 500, seed = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)
  if (is.null(V(g)$name)) V(g)$name <- paste0("node_", seq_len(vcount(g)))
  n <- vcount(g)
  if (any(sizes < 1 | sizes > n | sizes != round(sizes))) {
    stop("sizes must be positive integers no larger than vcount(g) = ", n,
         ".", call. = FALSE)
  }

  random_groups <- unlist(
    lapply(sizes, function(sz) replicate(n_reps, sample(n, sz), simplify = FALSE)),
    recursive = FALSE
  )

  # include_intact = FALSE: a reference row for the intact network has no
  # place mixed into a null distribution's replicates (knockout_robustness()
  # defaults to including one for direct, human-facing calls).
  knockout_robustness(g, groups = random_groups, ..., include_intact = FALSE)
}

# --- 1e. Compare an observed knock-out against the null (size-matched) -----
#' Compare an observed knock-out against a null distribution
#'
#' Empirical p-value testing whether an observed group's knock-out effect
#' (from [knockout_robustness()]) is unusual compared to a null
#' distribution of *random* groups of the *same size*
#' ([knockout_robustness_null_removal()]) -- the [knockout_robustness()]
#' counterpart of [compare_to_null()], matched by group size instead of
#' by removal step.
#'
#' The p-value uses the same two-sided, mean-centred empirical test as
#' [compare_to_null()]: the fraction of same-size null replicates at
#' least as far from their own mean as the observed group's value is.
#'
#' @param observed Output of [knockout_robustness()] -- one or more
#'   groups to test. Its intact-network reference row, if present
#'   (`include_intact = TRUE`, the default), is dropped automatically with
#'   a warning -- there's no meaningful null of size 0 to compare it
#'   against.
#' @param null_df Output of [knockout_robustness_null_removal()]. Must
#'   include every group size present in `observed` -- an observed group
#'   whose size has no matching null replicates errors, since the p-value
#'   would be undefined, not just imprecise.
#' @param index Character vector of indicator column name(s) to compare.
#'   `NULL` (default) uses every indicator column present in `observed`.
#'
#' @return A `data.frame` with columns `group_id`, `group`, `Index`,
#'   `n_removed`, `observed`, `null_mean` (same-size null replicates only),
#'   and `p_value` -- one row per observed group per indicator in `index`.
#'
#' @seealso [knockout_robustness()], [knockout_robustness_null_removal()],
#'   [plot_knockout_null()], [compare_to_null()]
#' @export
compare_knockout_to_null <- function(observed, null_df, index = NULL) {
  observed <- .drop_intact_row(observed)
  idx_cols <- setdiff(names(observed), c("group_id", "group", "n_removed"))
  if (is.null(index)) index <- idx_cols
  unknown_obs <- setdiff(index, idx_cols)
  if (length(unknown_obs) > 0) {
    stop("index has name(s) not found in observed: ", paste(unknown_obs, collapse = ", "),
         ".", call. = FALSE)
  }
  unknown_null <- setdiff(index, names(null_df))
  if (length(unknown_null) > 0) {
    stop("index has name(s) not found in null_df: ", paste(unknown_null, collapse = ", "),
         ".", call. = FALSE)
  }
  missing_sizes <- setdiff(unique(observed$n_removed), unique(null_df$n_removed))
  if (length(missing_sizes) > 0) {
    stop("observed has group size(s) with no matching null replicates in ",
         "null_df: ", paste(missing_sizes, collapse = ", "), ". Build ",
         "knockout_robustness_null_removal(g, sizes = ...) including ",
         "these size(s) first.", call. = FALSE)
  }

  out <- lapply(index, function(ix) {
    p <- vapply(seq_len(nrow(observed)), function(i) {
      sz <- observed$n_removed[i]
      obs_val <- observed[[ix]][i]
      null_vals <- null_df[[ix]][null_df$n_removed == sz]
      if (length(null_vals) == 0 || is.na(obs_val)) return(NA_real_)
      center <- mean(null_vals, na.rm = TRUE)
      mean(abs(null_vals - center) >= abs(obs_val - center), na.rm = TRUE)
    }, numeric(1))

    null_mean <- vapply(observed$n_removed, function(sz) {
      mean(null_df[[ix]][null_df$n_removed == sz], na.rm = TRUE)
    }, numeric(1))

    data.frame(
      group_id  = observed$group_id,
      Index     = ix,
      n_removed = observed$n_removed,
      observed  = observed[[ix]],
      null_mean = unname(null_mean),
      p_value   = p
    )
  })
  out <- do.call(rbind, out)
  out$group <- rep(observed$group, length(index))
  out[, c("group_id", "group", "Index", "n_removed", "observed", "null_mean", "p_value")]
}

# --- 2. Summarise the null into a quantile envelope per step ---------------
#' Summarise a null distribution into a quantile envelope
#'
#' Collapses the many replicates from [robustness_null_removal()] or
#' [robustness_null_structural()] into, by default, a 95% envelope (2.5% /
#' 50% / 97.5% quantiles) plus the mean, per indicator and per number of
#' nodes removed -- ready to hand to [plot_robustness()]'s `null_envelope`
#' argument as a shaded reference band (the `median` column is what
#' [plot_robustness()] draws as the representative "Failures" line).
#'
#' @param null_df Output of [robustness_null_removal()] or
#'   [robustness_null_structural()] (wide format, one column per indicator
#'   plus `n_removed` and `rep`).
#' @param probs Named numeric vector of three quantile *levels* (each in
#'   `[0, 1]`) to compute -- default `c(lower = 0.025, median = 0.5, upper =
#'   0.975)`, the 2.5th/50th/97.5th percentiles across replicates (a 95%
#'   envelope: `upper - lower = 0.95`). The names -- not `quantile()`'s own
#'   auto-generated labels -- are what determine the output column names
#'   (`lower`/`median`/`upper`), so changing `probs=` changes which
#'   quantiles those *columns* hold without changing their names -- e.g.
#'   `probs = c(lower = 0.1, median = 0.5, upper = 0.9)` gives an 80%
#'   envelope, still reported in columns called `lower`/`upper`. The levels
#'   actually used are attached to the return value as `attr(result,
#'   "probs")`, which [plot_robustness()] reads to caption the envelope's
#'   true coverage instead of assuming 95%.
#'
#' @return A `data.frame` with columns `Index`, `n_removed`, `mean`, `lower`,
#'   `median`, `upper` -- long format, one row per indicator per removal
#'   step. `lower`/`median`/`upper` are the quantiles named by `probs=`
#'   (2.5%/50%/97.5% by default) -- see `probs=` above for what those
#'   column names actually mean numerically, and where to find the exact
#'   levels used (`attr(result, "probs")`).
#'
#' @seealso [robustness_null_removal()], [robustness_null_structural()],
#'   [plot_robustness()]
#' @export
summarise_null <- function(null_df, probs = c(lower = 0.025, median = 0.5, upper = 0.975)) {
  # Requires: dplyr, tidyr (already used elsewhere in the project)
  idx_cols <- setdiff(names(null_df), c("n_removed", "rep"))

  out <- null_df |>
    tidyr::pivot_longer(cols = tidyr::all_of(idx_cols), names_to = "Index", values_to = "Value") |>
    dplyr::group_by(Index, n_removed) |>
    dplyr::summarise(
      mean   = mean(Value, na.rm = TRUE),
      lower  = stats::quantile(Value, probs["lower"],  na.rm = TRUE),
      median = stats::quantile(Value, probs["median"], na.rm = TRUE),
      upper  = stats::quantile(Value, probs["upper"],  na.rm = TRUE),
      .groups = "drop"
    )
  attr(out, "probs") <- probs
  out
}

# --- 3. Compare an observed curve (e.g. an attack) against the null --------
#' Empirical p-value of an observed curve against a null distribution
#'
#' For every removal step and every indicator, computes the two-sided
#' empirical p-value: the fraction of null replicates (from
#' [robustness_null_removal()]) at least as far from the null's own mean as the
#' observed value is. A small p-value at a given step means the observed
#' curve (e.g. a targeted attack) is unusually extreme compared to what
#' random failures alone would produce at that same number of nodes
#' removed.
#'
#' @param observed Output of [robustness()] for the scenario you want to
#'   test (e.g. an `attack_by_*()` curve).
#' @param null_df Output of [robustness_null_removal()] (the raw, wide-format
#'   replicates -- not the [summarise_null()] envelope). Must have been
#'   built with the same indicators as `observed` (see [robustness_null_removal()]'s
#'   `...` argument) -- if `observed` has an indicator column `null_df`
#'   doesn't, this errors rather than silently comparing only the columns
#'   that happen to match.
#'
#' @return A `data.frame` with columns `n_removed`, `Index`, `observed`, `p_value`,
#'   one row per indicator per removal step.
#'
#' @seealso [robustness_null_removal()], [robustness()]
#' @export
compare_to_null <- function(observed, null_df) {
  idx_cols <- setdiff(names(observed), "n_removed")
  missing_cols <- setdiff(idx_cols, names(null_df))
  if (length(missing_cols) > 0) {
    stop("observed has indicator(s) not present in null_df: ",
         paste(missing_cols, collapse = ", "), ". observed and null_df must ",
         "be built with the same indicators for the comparison to be valid ",
         "-- pass the same keep=/exclude=/custom=/directed=/... you used ",
         "for observed to robustness_null_removal() as well (it forwards them via ",
         "... straight through to robustness()).", call. = FALSE)
  }

  out <- lapply(idx_cols, function(col) {
    p <- vapply(seq_len(nrow(observed)), function(i) {
      n_i <- observed$n_removed[i]
      null_vals <- null_df[[col]][null_df$n_removed == n_i]
      obs_val <- observed[[col]][i]
      if (length(null_vals) == 0 || is.na(obs_val)) return(NA_real_)
      center <- mean(null_vals, na.rm = TRUE)
      mean(abs(null_vals - center) >= abs(obs_val - center), na.rm = TRUE)
    }, numeric(1))
    data.frame(n_removed = observed$n_removed, Index = col, observed = observed[[col]], p_value = p)
  })
  do.call(rbind, out)
}

# --- 4. Single-number robustness index (area under the curve) --------------
#' Robustness index (normalised area under an indicator's curve)
#'
#' Generalises the R-index of Schneider, Moreira, Andrade, Havlin & Herrmann
#' (2011) <doi:10.1073/pnas.1009440108> -- originally defined for the giant
#' component -- to any indicator column: the mean of an indicator's curve
#' across the removal sequence, normalised to its intact-network value.
#'
#' \deqn{R = \frac{1}{N}\sum_{Q=1}^{N} \frac{X(Q)}{X(0)}}{R = mean(X(Q) / X(0))}
#'
#' where \eqn{X(Q)}{X(Q)} is the indicator's value after \eqn{Q}{Q} vertices
#' have been removed, \eqn{X(0)}{X(0)} is its intact-network value, and
#' \eqn{N}{N} is the number of removal steps.
#'
#' `R` describes how far an indicator's value, on average across the whole
#' removal sequence, sits from its intact-network baseline -- deliberately
#' neutral about whether that shift is desirable. For an indicator that can
#' only shrink as vertices are removed (e.g. `giant_component_size`, `connectivity`), `R` is
#' bounded in `[0, 1]`. Not every indicator is bounded this way: `diameter`/
#' `mean_distance`, for instance, typically *increase* before a network fragments
#' (removing hubs forces remaining paths to route around them), so their
#' `R` can exceed 1. Whether a higher or lower value matters, and in which
#' direction, depends on what the indicator means for the network and
#' question you're studying -- not on this function.
#'
#' @param res Output of [robustness()] for one scenario. Must be a
#'   `data.frame` -- if it isn't (e.g. a plain string landed here because
#'   an argument was misplaced when building `scenario_list` for
#'   [compare_robustness()]), this errors with a message pointing at that
#'   likely cause rather than a confusing "column not found".
#' @param index Character vector of indicator column name(s) to summarise.
#'   `NULL` (default) uses every indicator column present in `res`. If `res`
#'   doesn't have a name you asked for (e.g. it was excluded via
#'   `exclude=`/`keep=` when calling [robustness()]), this errors rather
#'   than silently returning `NaN`.
#'
#' @return A single numeric value when `index` resolves to exactly one
#'   indicator (the historical behaviour, including the `index = NULL`
#'   default on a `res` with only one indicator column); a named numeric
#'   vector, one entry per indicator, otherwise.
#'
#' @seealso [robustness()], [compare_robustness()], [robustness_index_null()],
#'   [compare_robustness_index_to_null()]
#' @export
robustness_index <- function(res, index = NULL) {
  if (!is.data.frame(res)) {
    stop("res must be a data.frame returned by robustness() -- got a ",
         paste(class(res), collapse = "/"), ". This usually means an argument ",
         "landed in the wrong place, e.g. index= accidentally placed inside ",
         "the list() passed to compare_robustness() instead of after it.",
         call. = FALSE)
  }
  if (!("n_removed" %in% names(res))) {
    stop("res has no n_removed column. Curves from netRobust < 0.3.0 (with an ",
         "`N` column instead) must be recomputed with robustness()/",
         "edge_robustness().", call. = FALSE)
  }
  if (is.null(index)) index <- setdiff(names(res), "n_removed")
  unknown <- setdiff(index, names(res))
  if (length(unknown) > 0) {
    stop("index has name(s) not found in res: ", paste(unknown, collapse = ", "),
         ". Available columns: ", paste(setdiff(names(res), "n_removed"), collapse = ", "),
         ".", call. = FALSE)
  }
  steps <- res$n_removed < max(res$n_removed)
  result <- vapply(index, function(ix) {
    vals <- res[[ix]][steps]
    mean(vals / vals[1], na.rm = TRUE)
  }, numeric(1))
  if (length(result) == 1) unname(result) else result
}

#' Compare robustness indices across several removal scenarios
#'
#' Applies [robustness_index()] to a named list of [robustness()] results
#' (e.g. random failures vs. several targeted attack criteria) and
#' collects them into a single comparison table.
#'
#' @param scenario_list A named list of [robustness()] outputs, e.g.
#'   `list(Failures = failure, Degree = attack, PageRank = resilience_pr)`.
#' @param index Character vector of indicator column name(s) to summarise.
#'   `NULL` (default) uses every indicator column common to *all* scenarios
#'   in `scenario_list` (their `robustness()` calls need not have used the
#'   same `keep=`/`exclude=`) -- errors if there's no overlap at all.
#'
#' @return A `data.frame` with columns `Scenario`, `Index`, and `R_index`,
#'   one row per scenario per indicator.
#'
#' @seealso [robustness_index()], [robustness()]
#' @export
compare_robustness <- function(scenario_list, index = NULL) {
  if (is.null(index)) {
    cols_per_scenario <- lapply(scenario_list, function(x) {
      if (!is.data.frame(x)) {
        stop("every element of scenario_list must be a data.frame returned ",
             "by robustness() -- got a ", paste(class(x), collapse = "/"),
             ". This usually means an argument landed in the wrong place, ",
             "e.g. index= accidentally placed inside the list() instead of ",
             "after it.", call. = FALSE)
      }
      setdiff(names(x), "n_removed")
    })
    index <- Reduce(intersect, cols_per_scenario)
    if (length(index) == 0) {
      stop("scenario_list's elements share no common indicator columns -- ",
           "pass index= explicitly to pick one/some that are present in ",
           "every scenario.", call. = FALSE)
    }
  }

  out <- lapply(index, function(ix) {
    r_index <- vapply(names(scenario_list), function(nm) {
      tryCatch(
        robustness_index(scenario_list[[nm]], index = ix),
        error = function(e) stop("scenario \"", nm, "\": ", conditionMessage(e), call. = FALSE)
      )
    }, numeric(1))
    data.frame(Scenario = names(scenario_list), Index = ix, R_index = r_index, row.names = NULL)
  })
  do.call(rbind, out)
}

# --- 5. Per-replicate R-index (internal, shared) ----------------------------
# Splits a null distribution into its replicates and computes robustness_index()
# on each one, for every requested indicator -- the shared computation behind
# robustness_index_null(), compare_robustness_index_to_null(), and
# plot_r_index_null(). Not exported: the per-replicate values aren't meant to
# be a user-facing deliverable (there's nothing to read off ~500 raw numbers
# directly), they're the raw material each of those three functions
# summarises for its own purpose.
#
# The split() is the expensive part of this -- done ONCE here and reused for
# every index requested, rather than each caller (or, before index= accepted
# more than one, each separate call) re-splitting null_df from scratch.
#
# Detects the most likely way to end up here by mistake: passing
# summarise_null()'s output (long format, an `Index` column, no `rep`
# column) instead of the raw null distribution from robustness_null_removal()/
# robustness_null_structural() (wide format, one column per indicator, a
# `rep` column). Both objects get called "null_something" in typical code,
# so this is an easy mix-up -- worth a specific message instead of the
# generic "column not found" that `index` not matching would otherwise give.
#
# Returns an n_reps x length(index) matrix (one column per indicator, named),
# regardless of length(index) -- callers colMeans()/index by column name
# without needing to special-case a single index into a bare vector.
.robustness_index_per_replicate <- function(null_df, index = NULL) {
  if ("Index" %in% names(null_df) && !("rep" %in% names(null_df))) {
    stop("null_df looks like the output of summarise_null() (it has an ",
         "`Index` column but no `rep` column), not a raw null distribution. ",
         "Pass the object returned by robustness_null_removal() or ",
         "robustness_null_structural() instead (the one you'd pass TO ",
         "summarise_null(), not what it returns).", call. = FALSE)
  }
  idx_cols <- setdiff(names(null_df), c("n_removed", "rep"))
  if (is.null(index)) index <- idx_cols
  unknown <- setdiff(index, idx_cols)
  if (length(unknown) > 0) {
    stop("index has name(s) not found in null_df: ", paste(unknown, collapse = ", "),
         ". Available columns: ", paste(idx_cols, collapse = ", "), ".", call. = FALSE)
  }

  reps <- split(null_df, null_df$rep)
  # vapply(reps, ..., numeric(length(index))) would collapse to a plain
  # (wrongly-oriented) vector when length(index) == 1 -- vapply's own
  # simplification rule, not something t() then undoes correctly. Building
  # the matrix explicitly from a flat, row-major vector sidesteps that
  # entirely, for any length(index).
  values <- unlist(lapply(reps, function(rep_df) {
    vapply(index, function(ix) robustness_index(rep_df, index = ix), numeric(1))
  }), use.names = FALSE)
  mat <- matrix(values, nrow = length(reps), ncol = length(index), byrow = TRUE)
  colnames(mat) <- index
  mat
}

# --- 6. Null-distribution R-index (mean across replicates) -----------------
#' Null-distribution R-index (mean across replicates)
#'
#' Computes [robustness_index()] separately for each replicate in a null
#' distribution (from [robustness_null_removal()] or
#' [robustness_null_structural()]), then averages across replicates. This is
#' the appropriate way to get a single-number R-index baseline for a null
#' model -- see Details for why it's *not* equivalent to computing the
#' R-index of an already-averaged curve.
#'
#' \deqn{\bar{R} = \frac{1}{M}\sum_{m=1}^{M} R_m}{Rbar = mean(R_1, ..., R_M)}
#'
#' where \eqn{R_m}{R_m} is the R-index (see [robustness_index()]) of
#' replicate \eqn{m}{m} and \eqn{M}{M} is the number of replicates
#' (`n_reps`).
#'
#' @details
#' For [robustness_null_removal()] specifically (fixed network, only removal
#' order randomised), the intact-network value at `n_removed = 0` is identical
#' across every replicate, which makes the R-index a linear functional of
#' the curve -- so in that one case, \eqn{\bar{R}}{Rbar} happens to equal
#' `robustness_index()` computed on the mean curve. That equivalence breaks
#' for [robustness_null_structural()], where each replicate is a
#' *different* rewired graph (so the intact-network value varies by
#' replicate too). Averaging replicate-level R-index values, as this
#' function does, stays correct for both null models -- which is why it's
#' implemented this way rather than as a shortcut through an
#' already-averaged curve.
#'
#' Reporting the mean R-index across many random realisations is standard
#' practice in the R-index literature; this function is the multi-replicate
#' counterpart to [robustness_index()], meant to sit alongside
#' [compare_robustness()] (which summarises single, deterministic attack
#' curves) when building a scenario comparison table that also includes a
#' randomised baseline. This gives you a single descriptive number, not a
#' significance test -- for an empirical p-value against this same null
#' distribution, see [compare_robustness_index_to_null()].
#'
#' @param null_df Output of [robustness_null_removal()] or
#'   [robustness_null_structural()] (wide format, with a `rep` column).
#' @param index Character vector of indicator column name(s) to summarise.
#'   `NULL` (default) uses every indicator column present in `null_df`.
#'
#' @return A single numeric value (the mean R-index across replicates) when
#'   `index` resolves to exactly one indicator; a named numeric vector, one
#'   entry per indicator, otherwise.
#'
#' @seealso [robustness_index()], [compare_robustness()],
#'   [compare_robustness_index_to_null()], [plot_r_index_null()],
#'   [robustness_null_removal()], [robustness_null_structural()]
#' @export
robustness_index_null <- function(null_df, index = NULL) {
  mat <- .robustness_index_per_replicate(null_df, index = index)
  result <- colMeans(mat, na.rm = TRUE)
  if (length(result) == 1) unname(result) else result
}

# --- 7. Compare an observed R-index against the null (global test) ---------
#' Compare an observed R-index against a null distribution
#'
#' Empirical p-value testing whether an observed scenario's R-index (from
#' [robustness_index()]) is unusual compared to what a null model
#' ([robustness_null_removal()]/[robustness_null_structural()]) would
#' produce. This is the R-index counterpart to [compare_to_null()]'s
#' pointwise, per-removal-step test, and answers a different question:
#' not "at which removal steps does the curve differ from the null?"
#' (that's [compare_to_null()]) but "does the whole curve, summarised as
#' one number, differ from the null overall?" The two are complementary --
#' see [robustness_index()] for why collapsing a curve to one number loses
#' information about *where* along the curve a difference happens, even
#' when it correctly detects *whether* there is one.
#'
#' The p-value uses the same two-sided, mean-centred empirical test as
#' [compare_to_null()]: the fraction of the null's per-replicate R-index
#' values at least as far from their own mean as `observed`'s R-index is.
#'
#' @param observed Output of [robustness()] for the scenario being tested
#'   (e.g. an `attack_by_*()` curve).
#' @param null_df Output of [robustness_null_removal()] or
#'   [robustness_null_structural()] (the raw, wide-format replicates --
#'   not [summarise_null()]'s output; passing that by mistake is a common
#'   mix-up and errors with a specific message pointing at the fix).
#' @param index Character vector of indicator column name(s) to summarise.
#'   `NULL` (default) uses every indicator column present in `observed`.
#'
#' @return A `data.frame` with columns `Index`, `observed` (the observed
#'   R-index), `null_mean` (the null distribution's mean R-index -- what
#'   the p-value is centred on), `null_median` (a descriptive complement;
#'   if it differs a lot from `null_mean`, that itself signals the null
#'   R-index distribution is skewed), and `p_value` -- one row per indicator
#'   in `index`.
#'
#' @seealso [robustness_index()], [robustness_index_null()],
#'   [compare_to_null()], [plot_r_index_null()]
#' @export
compare_robustness_index_to_null <- function(observed, null_df, index = NULL) {
  if (is.null(index)) index <- setdiff(names(observed), "n_removed")
  null_mat <- .robustness_index_per_replicate(null_df, index = index)

  out <- lapply(index, function(ix) {
    obs_index <- robustness_index(observed, index = ix)
    null_vals <- null_mat[, ix]
    center <- mean(null_vals, na.rm = TRUE)
    p <- mean(abs(null_vals - center) >= abs(obs_index - center), na.rm = TRUE)
    data.frame(
      Index       = ix,
      observed    = obs_index,
      null_mean   = center,
      null_median = stats::median(null_vals, na.rm = TRUE),
      p_value     = p
    )
  })
  do.call(rbind, out)
}
