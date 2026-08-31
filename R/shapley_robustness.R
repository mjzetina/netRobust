#### SHAPLEY ROBUSTNESS ####
# Manuel J. Zetina-Rejon
# Shapley-value node importance: fair allocation of each robustness
# indicator's total change across the removal sequence to the individual
# nodes responsible for it, following the cooperative-game-theoretic
# framework of Shapley (1953) and its Monte Carlo approximation via random
# permutation sampling (the standard practice when exact Shapley values are
# intractable -- n! orderings even for a modest-sized network).
#
# A node's Shapley value for a given indicator is its average marginal
# contribution across every possible removal order: how much that
# indicator changed, on average, at the exact moment that node was removed,
# averaged over all the different "contexts" (which other nodes were
# already gone) it could have been removed in. random_failures() + a full
# removal curve already gives this for FREE from a single simulation pass:
# the marginal contribution of the node removed at step k is just the
# difference between the indicator's value right before and right after
# that one removal. n_reps random orders, averaged, approximate the true
# Shapley value for every node simultaneously -- one simulation pass per
# replicate, not one per node.

# --- Internal: full (n+1)-row removal curve, indexed by removal step -------
# Like .removal_curve() (robustness.R), but keeps the fully-emptied
# network's row (dropped there) -- shapley_robustness() needs it to compute
# the LAST removed node's marginal contribution (from 1 node left to 0),
# and doesn't need the N column .removal_curve() adds for robustness()'s
# output.
.shapley_curve <- function(g, removal_names, indicators) {
  n <- length(removal_names)
  rows <- vector("list", n + 1)
  rows[[1]] <- .compute_indicator_row(g, indicators)

  gi <- g
  for (i in seq_len(n)) {
    gi <- delete_vertices(gi, removal_names[i])
    rows[[i + 1]] <- .compute_indicator_row(gi, indicators)
  }

  do.call(rbind.data.frame, rows)
}

#' Shapley-value node importance
#'
#' Estimates each node's Shapley value (Shapley 1953) for every robustness
#' indicator: its average marginal contribution to that indicator, across
#' `n_reps` random removal orders. This is a fair, game-theoretic notion of
#' "importance" -- unlike a single attack curve (which only shows what
#' happens when nodes are removed in ONE particular order, e.g. by
#' decreasing degree), a Shapley value credits each node for its impact
#' averaged over every context it could be removed in, alone or alongside
#' any combination of other removed nodes.
#'
#' Computed efficiently: a single random removal order already gives every
#' node's marginal contribution for free (the change in an indicator
#' between consecutive steps of that one simulation), so `n_reps` random
#' orders -- the same simulation [random_failures()] + [robustness()]
#' already run -- estimate every node's Shapley value simultaneously, in
#' one pass per replicate, rather than requiring a separate simulation per
#' node.
#'
#' Alongside the per-node estimates, a single global permutation test asks
#' whether nodes differ from each other more than sampling noise alone
#' would produce -- deliberately NOT a node-vs-node battery of pairwise
#' tests (`choose(vcount(g), 2)` of them for even a modest network), which
#' would both be impractical at scale and reintroduce the same
#' multiple-comparisons problem [compare_robustness_index_to_null()] avoids
#' by testing the whole curve at once instead of step by step. See
#' Details.
#'
#' @details
#' # The global test
#' Within a single replicate, a node's marginal contribution is exchangeable
#' with any other node's under the null hypothesis that node identity
#' doesn't matter (only which position in that replicate's removal order
#' was occupied). The test statistic is the variance, across nodes, of
#' their mean marginal contribution; its null distribution is built by
#' permuting which node "owns" each replicate's row of contributions --
#' the same permutation applied across every indicator at once, since a
#' node's row of contributions across indicators all came from removing
#' that one node at that one step in that one replicate -- `n_perm` times,
#' recomputing the statistic each time. The empirical p-value is the
#' fraction of permuted statistics at least as large as the observed one:
#' a large observed statistic means nodes are more spread out (more
#' differentiated from each other) than chance alone predicts.
#'
#' This tells you *whether* nodes differ at all, in one test per indicator
#' -- not *which* nodes differ from which. For that, the per-node standard
#' error already in `$nodes` supports informal visual comparison (e.g. via
#' [plot_shapley()]'s error bars) without needing a battery of formal
#' pairwise tests.
#'
#' @param g An `igraph` object.
#' @param n_reps Number of random removal orders to average over. Default
#'   `500`, matching [robustness_null_removal()]'s convention.
#' @param n_perm Number of node-identity permutations for the global test.
#'   Default `500`. Larger values give finer p-value resolution at the
#'   cost of runtime.
#' @param seed Optional integer passed to `set.seed()` for reproducibility.
#' @param indicators,exclude,keep,custom,optional,directed,component_mode,mode,weights,weights_as
#'   As in [robustness()] -- forwarded to the same internal setup that
#'   resolves the indicator set and directed/weight treatment, so a
#'   Shapley value is computed for the same indicators, on the same terms,
#'   that [robustness()] would use.
#'
#' @return An object of class `"shapley_robustness"`: a list with
#'   \describe{
#'     \item{`nodes`}{A `data.frame`, one row per vertex, with `node` and,
#'       for every indicator, a value column and a `<indicator>_se`
#'       standard-error column (the standard error of the mean across
#'       `n_reps` replicates).}
#'     \item{`global_test`}{A `data.frame`, one row per indicator, with
#'       `Index`, `observed_stat`, and `p_value` (see Details).}
#'   }
#'
#' @seealso [plot_shapley()], [robustness()], [random_failures()],
#'   [compare_robustness_index_to_null()]
#' @export
shapley_robustness <- function(g, n_reps = 500, n_perm = 500, seed = NULL,
                                indicators = NULL, exclude = NULL, keep = NULL,
                                custom = NULL, optional = NULL,
                                directed = NULL, component_mode = "weak", mode = NULL,
                                weights = NULL, weights_as = NULL) {
  setup <- .resolve_robustness_setup(g, indicators, exclude, keep, custom, optional,
                                      directed, component_mode, mode, weights, weights_as)
  g <- setup$g
  if (is.null(V(g)$name)) V(g)$name <- paste0("node_", seq_len(vcount(g)))
  node_names <- V(g)$name
  n <- length(node_names)
  ind_names <- names(setup$indicators)

  contrib <- array(NA_real_, dim = c(n_reps, n, length(ind_names)),
                    dimnames = list(NULL, node_names, ind_names))

  if (!is.null(seed)) set.seed(seed)
  for (r in seq_len(n_reps)) {
    ord <- sample(node_names)
    curve <- .shapley_curve(g, ord, setup$indicators)
    before <- as.matrix(curve[seq_len(n), , drop = FALSE])
    after  <- as.matrix(curve[seq_len(n) + 1, , drop = FALSE])
    contrib[r, ord, ] <- before - after
    .console_progress(r, n_reps)
  }

  node_mean <- apply(contrib, c(2, 3), mean, na.rm = TRUE)
  node_se   <- apply(contrib, c(2, 3), function(x) stats::sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))

  nodes_df <- data.frame(node = node_names, stringsAsFactors = FALSE)
  for (nm in ind_names) {
    nodes_df[[nm]] <- unname(node_mean[, nm])
    nodes_df[[paste0(nm, "_se")]] <- unname(node_se[, nm])
  }
  rownames(nodes_df) <- NULL

  # Global test: permute node identity (one permutation per replicate,
  # shared across every indicator so a node's contributions move together)
  # and recompute the observed statistic's null distribution -- see the
  # "The global test" section above.
  observed_stat <- apply(node_mean, 2, stats::var)
  null_stat <- matrix(NA_real_, nrow = n_perm, ncol = length(ind_names),
                       dimnames = list(NULL, ind_names))
  permuted <- array(NA_real_, dim = dim(contrib), dimnames = dimnames(contrib))
  for (p in seq_len(n_perm)) {
    for (r in seq_len(n_reps)) {
      permuted[r, , ] <- contrib[r, sample(n), ]
    }
    perm_mean <- apply(permuted, c(2, 3), mean, na.rm = TRUE)
    null_stat[p, ] <- apply(perm_mean, 2, stats::var)
    .console_progress(p, n_perm)
  }

  p_value <- vapply(seq_along(ind_names), function(k) {
    mean(null_stat[, k] >= observed_stat[k], na.rm = TRUE)
  }, numeric(1))

  global_test <- data.frame(
    Index = ind_names,
    observed_stat = unname(observed_stat),
    p_value = p_value,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  structure(list(nodes = nodes_df, global_test = global_test), class = "shapley_robustness")
}
