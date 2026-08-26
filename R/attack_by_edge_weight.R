#### ATTACK BY EDGE WEIGHT ####
#' Targeted-attack removal order by edge weight
#'
#' Edges are removed in order of `E(g)$weight` -- strongest ties first by
#' default. Unlike [attack_by_edge_betweenness()], this has no non-weighted
#' fallback: weight *is* the criterion, so `g` must actually be weighted.
#'
#' @param g An `igraph` object. Must be weighted (`igraph::is_weighted(g)`)
#'   -- errors otherwise, since there's no sensible unweighted interpretation
#'   to fall back to (contrast with `weights_as=` elsewhere in the package,
#'   which has one).
#' @param decreasing Logical. `TRUE` (default) removes the strongest/
#'   heaviest ties first -- e.g. to ask "how robust is the network to
#'   losing its most important relationships?". `FALSE` removes the
#'   weakest ties first instead -- e.g. to simulate gradual attrition of
#'   marginal connections.
#' @param on_parallel How to handle parallel edges (multiple edges between
#'   the same pair of vertices). `"collapse"` (default) merges them via
#'   `igraph::simplify()`, summing weights, with a warning. `"error"` stops
#'   instead. See `.prepare_edge_graph()`.
#'
#' @return A named integer vector in removal order, as produced by
#'   [edge_removal_order()] -- ready to pass to [edge_robustness()].
#'
#' @seealso [edge_removal_order()], [random_edge_failures()],
#'   [attack_by_edge_betweenness()], [edge_robustness()]
#' @export
attack_by_edge_weight <- function(g, decreasing = TRUE, on_parallel = c("collapse", "error")) {
  if (!is_weighted(g)) {
    stop("attack_by_edge_weight() requires a weighted graph (E(g)$weight); ",
         "g has no edge weights. Use attack_by_edge_betweenness() or ",
         "random_edge_failures() instead if your network is unweighted.",
         call. = FALSE)
  }
  g <- .prepare_edge_graph(g, on_parallel = on_parallel)
  edge_removal_order(g, E(g)$weight, decreasing = decreasing)
}
