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
# Requires: igraph (already used elsewhere). Uses future.apply for
# parallel replicates if installed; falls back to plain lapply otherwise.
#   install.packages("future.apply")

# --- 1. Build the null distribution ----------------------------------------
#' Monte Carlo null distribution of robustness curves
#'
#' Runs `n_reps` independent random-failure simulations of the same network
#' (via [random_failures()] + [robustness()]), producing the empirical null
#' distribution of every robustness indicator at every removal step. This is
#' the statistical baseline against which a targeted attack curve can be
#' judged: if a single random draw is unusual to expect, thousands of them
#' trace out the range of outcomes chance alone would produce.
#'
#' @param g An `igraph` object.
#' @param n_reps Number of random-failure replicates. Rubio's thesis (this
#'   project's methodological source for this framework) uses 10,000 for
#'   final results; lower values (the default 500) are fine for quick
#'   exploration.
#' @param parallel If `TRUE` (default) and the `future.apply` package is
#'   installed, replicates run in parallel across whatever backend was set
#'   with `future::plan()` (e.g. `future::plan(future::multisession)`).
#'   Falls back to sequential `lapply()` if `future.apply` isn't installed,
#'   with no error.
#' @param seed Optional integer passed to `set.seed()` for reproducibility.
#' @param ... Forwarded as-is to every internal [robustness()] call (e.g.
#'   `indicators=`, `exclude=`, `keep=`, `custom=`, `directed=`,
#'   `component_mode=`, `mode=`, `weights=`, `weights_as=`). This matters:
#'   for the null distribution to be a valid statistical baseline for an
#'   observed attack/failure curve, it must be built with the *exact same*
#'   settings as that curve -- same indicators, same directed/weights
#'   treatment. Pass here whatever extra arguments you used when computing
#'   the scenario you're testing against, e.g.
#'   `robustness_null(g, keep = c("GC", "APL"), directed = TRUE)` to match
#'   `robustness(g, ids, keep = c("GC", "APL"), directed = TRUE)`.
#'
#' @return A `data.frame` stacking all replicates: same columns as
#'   [robustness()]'s output, plus `rep` identifying which replicate each
#'   row came from. Pass this to [summarise_null()] or [compare_to_null()].
#'
#' @seealso [robustness()], [random_failures()], [summarise_null()],
#'   [compare_to_null()]
#' @export
robustness_null <- function(g, n_reps = 500, parallel = TRUE, seed = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)

  run_one <- function(i) {
    ids <- random_failures(g)
    res <- robustness(g, ids, ...)
    res$rep <- i
    res
  }

  use_parallel <- parallel && requireNamespace("future.apply", quietly = TRUE)
  reps <- if (use_parallel) {
    future.apply::future_lapply(seq_len(n_reps), run_one, future.seed = TRUE)
  } else {
    lapply(seq_len(n_reps), run_one)
  }

  do.call(rbind, reps)
}
# To run replicates in parallel across cores:
#   future::plan(future::multisession)   # once, at the top of main.R

# --- 2. Summarise the null into a quantile envelope per N ------------------
#' Summarise a null distribution into a quantile envelope
#'
#' Collapses the many replicates from [robustness_null()] into, by default,
#' a 95% envelope (2.5% / 50% / 97.5% quantiles) per indicator and per
#' number of nodes removed -- ready to hand to [plot_robustness()]'s
#' `null_envelope` argument as a shaded reference band.
#'
#' @param null_df Output of [robustness_null()] (wide format, one column
#'   per indicator plus `N` and `rep`).
#' @param probs Named numeric vector of three quantiles to compute. Must be
#'   named `lower`, `median`, `upper` (the names -- not `quantile()`'s own
#'   auto-generated labels -- are what determine the output column names).
#'
#' @return A `data.frame` with columns `Index`, `N`, `lower`, `median`,
#'   `upper` -- long format, one row per indicator per removal step.
#'
#' @seealso [robustness_null()], [plot_robustness()]
#' @export
summarise_null <- function(null_df, probs = c(lower = 0.025, median = 0.5, upper = 0.975)) {
  # Requires: dplyr, tidyr (already used elsewhere in the project)
  idx_cols <- setdiff(names(null_df), c("N", "rep"))

  null_df |>
    tidyr::pivot_longer(cols = tidyr::all_of(idx_cols), names_to = "Index", values_to = "Value") |>
    dplyr::group_by(Index, N) |>
    dplyr::summarise(
      lower  = stats::quantile(Value, probs["lower"],  na.rm = TRUE),
      median = stats::quantile(Value, probs["median"], na.rm = TRUE),
      upper  = stats::quantile(Value, probs["upper"],  na.rm = TRUE),
      .groups = "drop"
    )
}

# --- 3. Compare an observed curve (e.g. an attack) against the null --------
#' Empirical p-value of an observed curve against a null distribution
#'
#' For every removal step and every indicator, computes the two-sided
#' empirical p-value: the fraction of null replicates (from
#' [robustness_null()]) at least as far from the null's own mean as the
#' observed value is. A small p-value at a given step means the observed
#' curve (e.g. a targeted attack) is unusually extreme compared to what
#' random failures alone would produce at that same number of nodes
#' removed.
#'
#' @param observed Output of [robustness()] for the scenario you want to
#'   test (e.g. an `attack_by_*()` curve).
#' @param null_df Output of [robustness_null()] (the raw, wide-format
#'   replicates -- not the [summarise_null()] envelope). Must have been
#'   built with the same indicators as `observed` (see [robustness_null()]'s
#'   `...` argument) -- if `observed` has an indicator column `null_df`
#'   doesn't, this errors rather than silently comparing only the columns
#'   that happen to match.
#'
#' @return A `data.frame` with columns `N`, `Index`, `observed`, `p_value`,
#'   one row per indicator per removal step.
#'
#' @seealso [robustness_null()], [robustness()]
#' @export
compare_to_null <- function(observed, null_df) {
  idx_cols <- setdiff(names(observed), "N")
  missing_cols <- setdiff(idx_cols, names(null_df))
  if (length(missing_cols) > 0) {
    stop("observed has indicator(s) not present in null_df: ",
         paste(missing_cols, collapse = ", "), ". observed and null_df must ",
         "be built with the same indicators for the comparison to be valid ",
         "-- pass the same keep=/exclude=/custom=/directed=/... you used ",
         "for observed to robustness_null() as well (it forwards them via ",
         "... straight through to robustness()).", call. = FALSE)
  }

  out <- lapply(idx_cols, function(col) {
    p <- vapply(seq_len(nrow(observed)), function(i) {
      n_i <- observed$N[i]
      null_vals <- null_df[[col]][null_df$N == n_i]
      obs_val <- observed[[col]][i]
      if (length(null_vals) == 0 || is.na(obs_val)) return(NA_real_)
      center <- mean(null_vals, na.rm = TRUE)
      mean(abs(null_vals - center) >= abs(obs_val - center), na.rm = TRUE)
    }, numeric(1))
    data.frame(N = observed$N, Index = col, observed = observed[[col]], p_value = p)
  })
  do.call(rbind, out)
}

# --- 4. Single-number robustness index (area under the curve) --------------
#' Robustness index (normalised area under an indicator's curve)
#'
#' R-index style summary (Schneider et al. 2011): the mean of an
#' indicator's curve, normalised to its intact-network value. `1` means no
#' degradation at all across the whole removal sequence; closer to `0`
#' means the network collapses almost immediately. Useful to reduce a full
#' robustness curve to one comparable number per scenario.
#'
#' @param res Output of [robustness()] for one scenario. Must be a
#'   `data.frame` -- if it isn't (e.g. a plain string landed here because
#'   an argument was misplaced when building `scenario_list` for
#'   [compare_robustness()]), this errors with a message pointing at that
#'   likely cause rather than a confusing "column not found".
#' @param index Name of the indicator column to summarise (default `"GC"`,
#'   the giant component -- the most common choice in the literature). If
#'   `res` doesn't have this column (e.g. it was excluded via `exclude=`/
#'   `keep=` when calling [robustness()]), this errors rather than
#'   silently returning `NaN`.
#'
#' @return A single numeric value.
#'
#' @seealso [robustness()], [compare_robustness()]
#' @export
robustness_index <- function(res, index = "GC") {
  if (!is.data.frame(res)) {
    stop("res must be a data.frame returned by robustness() -- got a ",
         paste(class(res), collapse = "/"), ". This usually means an argument ",
         "landed in the wrong place, e.g. index= accidentally placed inside ",
         "the list() passed to compare_robustness() instead of after it.",
         call. = FALSE)
  }
  if (!index %in% names(res)) {
    stop("index = \"", index, "\" not found in res. Available columns: ",
         paste(setdiff(names(res), "N"), collapse = ", "), ".", call. = FALSE)
  }
  vals <- res[[index]]
  mean(vals / vals[1], na.rm = TRUE)
}

#' Compare robustness indices across several removal scenarios
#'
#' Applies [robustness_index()] to a named list of [robustness()] results
#' (e.g. random failures vs. several targeted attack criteria) and
#' collects them into a single comparison table.
#'
#' @param scenario_list A named list of [robustness()] outputs, e.g.
#'   `list(Failures = failure, Degree = attack, PageRank = resilience_pr)`.
#' @param index Name of the indicator column to summarise (default `"GC"`).
#'
#' @return A `data.frame` with columns `Scenario` and `R_index`, one row
#'   per element of `scenario_list`.
#'
#' @seealso [robustness_index()], [robustness()]
#' @export
compare_robustness <- function(scenario_list, index = "GC") {
  r_index <- vapply(names(scenario_list), function(nm) {
    tryCatch(
      robustness_index(scenario_list[[nm]], index = index),
      error = function(e) stop("scenario \"", nm, "\": ", conditionMessage(e), call. = FALSE)
    )
  }, numeric(1))
  data.frame(Scenario = names(scenario_list), R_index = r_index, row.names = NULL)
}
