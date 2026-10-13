#### ATTACK BY EDGE BETWEENNESS ####
#' Targeted-attack removal order by edge betweenness centrality
#'
#' Edges are removed in decreasing order of edge betweenness -- the "bridge"
#' relationships that lie on the most shortest paths -- first. The
#' edge-level counterpart of [attack_by_betweenness()]: attacks specific
#' relationships/ties, not the actors/nodes themselves.
#'
#' @param g An `igraph` object.
#' @param weights Optional numeric vector of edge weights, one per edge
#'   *after* any parallel-edge collapse (see `on_parallel`) -- if `g` has
#'   parallel edges and you pass an explicit vector, size it for the
#'   collapsed graph, not the original. `NULL` (default) and `g` weighted
#'   uses `weights_as` to decide how to interpret `E(g)$weight`.
#' @param weights_as `NULL` (default) uses edge betweenness's own native
#'   weight interpretation, `"distance"` -- lower weight means a shorter,
#'   more traversable path. Pass `"strength"` explicitly to invert (`1 /
#'   weight`) if your weights represent interaction strength instead (a
#'   higher value meaning a stronger tie).
#' @param on_parallel How to handle parallel edges (multiple edges between
#'   the same pair of vertices). `"collapse"` (default) merges them via
#'   `igraph::simplify()`, summing weights if any, with a warning.
#'   `"error"` stops instead. See `.prepare_edge_graph()`.
#' @param recalculate Logical. `FALSE` (default) ranks edges once, on the
#'   intact network (static attack). `TRUE` recomputes edge betweenness on
#'   the remaining network after every edge removal (adaptive attack); see
#'   [recalculated_attacks] for details and computational cost (roughly
#'   O(m^2 n) for a graph with *n* vertices and *m* edges).
#'
#' @return A named integer vector in removal order, as produced by
#'   [edge_removal_order()] -- ready to pass to [edge_robustness()].
#'
#' @seealso [edge_removal_order()], [resolve_weights()],
#'   [random_edge_failures()], [attack_by_edge_weight()], [edge_robustness()]
#' @export
attack_by_edge_betweenness <- function(g, weights = NULL, weights_as = NULL,
                                        on_parallel = c("collapse", "error"),
                                        recalculate = FALSE) {
  g <- .prepare_edge_graph(g, on_parallel = on_parallel)
  if (.check_recalculate(recalculate)) {
    static <- function(gi, w) attack_by_edge_betweenness(gi, weights = w, weights_as = weights_as)
    return(.adaptive_order(g, static, type = "edge", weights = weights))
  }
  weights <- resolve_weights(g, weights, weights_as, native = "distance")
  edge_removal_order(g, edge_betweenness(g, weights = weights), decreasing = TRUE)
}
