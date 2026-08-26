#### RANDOM EDGE FAILURES ####
#' Random edge-removal order (error/failure scenario)
#'
#' Generates a uniformly random edge removal order -- the edge-level
#' counterpart of [random_failures()]: edges disappear independently of any
#' structural property, representing loss of specific relationships without
#' the endpoint actors/nodes themselves disappearing.
#'
#' @param g An `igraph` object.
#' @param on_parallel How to handle parallel edges (multiple edges between
#'   the same pair of vertices), which would otherwise break the by-name
#'   edge identification this function relies on. `"collapse"` (default)
#'   merges them via `igraph::simplify()`, summing weights if any, with a
#'   warning. `"error"` stops instead. See `.prepare_edge_graph()`.
#'
#' @return A named integer vector in removal order, as produced by
#'   [edge_removal_order()] -- ready to pass to [edge_robustness()].
#'
#' @seealso [edge_removal_order()], [edge_robustness()],
#'   [attack_by_edge_betweenness()], [attack_by_edge_weight()]
#' @export
random_edge_failures <- function(g, on_parallel = c("collapse", "error")) {
  g <- .prepare_edge_graph(g, on_parallel = on_parallel)
  edge_removal_order(g, sample(ecount(g)), decreasing = TRUE)
}
