#### ATTACK BY EDGE WEIGHT ####
#' Targeted-attack removal order by edge weight
#'
#' Edges are removed in order of importance as derived from `E(g)$weight` --
#' the most important ties first by default (`decreasing = TRUE`). Unlike
#' [attack_by_edge_betweenness()], this has no non-weighted fallback: weight
#' *is* the criterion, so `g` must be weighted (or you must supply
#' `weights=` explicitly).
#'
#' @param g An `igraph` object.
#' @param weights Optional explicit numeric vector of edge weights, one per
#'   edge *after* any parallel-edge collapse (see `on_parallel`). If `NULL`
#'   (default), uses `E(g)$weight` -- `g` must then be weighted, or this
#'   errors.
#' @param weights_as Which way `E(g)$weight` (or `weights=`) should be read
#'   to decide "most important". `NULL` (default) and `"strength"` are
#'   equivalent here: the raw value *is* the importance (higher = more
#'   important, e.g. a diet fraction or a tie strength), so `decreasing =
#'   TRUE` removes the highest raw values first. Pass `"distance"`
#'   explicitly if your weights represent the opposite -- a cost/distance
#'   where a *lower* value means a stronger, more important tie (e.g.
#'   travel time, resistance) -- to invert (`1 / weight`) before ranking,
#'   so `decreasing = TRUE` still consistently means "most important tie
#'   first" regardless of which convention your weights follow.
#' @param decreasing Logical. `TRUE` (default) removes the most important
#'   ties first (per `weights_as`, above) -- e.g. to ask "how robust is
#'   the network to losing its most important relationships?". `FALSE`
#'   removes the least important ties first instead -- e.g. to simulate
#'   gradual attrition of marginal connections.
#' @param on_parallel How to handle parallel edges (multiple edges between
#'   the same pair of vertices). `"collapse"` (default) merges them via
#'   `igraph::simplify()`, summing weights, with a warning. `"error"` stops
#'   instead. See `.prepare_edge_graph()`.
#'
#' @return A named integer vector in removal order, as produced by
#'   [edge_removal_order()] -- ready to pass to [edge_robustness()].
#'
#' @seealso [edge_removal_order()], [resolve_weights()],
#'   [random_edge_failures()], [attack_by_edge_betweenness()],
#'   [edge_robustness()]
#' @export
attack_by_edge_weight <- function(g, weights = NULL, weights_as = NULL, decreasing = TRUE,
                                   on_parallel = c("collapse", "error")) {
  if (is.null(weights) && !is_weighted(g)) {
    stop("attack_by_edge_weight() requires a weighted graph (E(g)$weight) ",
         "or an explicit weights= vector; g has neither. Use ",
         "attack_by_edge_betweenness() or random_edge_failures() instead ",
         "if your network is unweighted.", call. = FALSE)
  }
  g <- .prepare_edge_graph(g, on_parallel = on_parallel)
  importance <- resolve_weights(g, weights, weights_as, native = "strength")
  edge_removal_order(g, importance, decreasing = decreasing)
}
