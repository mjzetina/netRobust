#### ATTACK BY BETWEENNESS ####
#' Targeted-attack removal order by betweenness centrality
#'
#' Vertices are removed in decreasing order of betweenness centrality --
#' the "brokers" or bridge nodes that lie on the most shortest paths --
#' first.
#'
#' @param g An `igraph` object.
#' @param directed Should edge direction be considered? `NULL` (default)
#'   auto-detects from `g` via `is_directed()`. Pass `FALSE` to
#'   deliberately treat a directed graph as undirected (e.g. to test
#'   whether direction matters for your question at all, by comparing the
#'   robustness index of the same network analysed both ways).
#' @param weights Optional numeric vector of edge weights. If `NULL`
#'   (default) and `g` is weighted, uses `weights_as` to decide how to
#'   interpret `E(g)$weight`; pass a vector explicitly to override both.
#' @param weights_as `NULL` (default) uses betweenness's own native weight
#'   interpretation, `"distance"` -- lower weight means a shorter, more
#'   traversable path, weights used as-is. If your weights instead
#'   represent *interaction strength* (e.g. a diet fraction, a tie
#'   strength) -- where a *higher* value means a stronger, more relevant
#'   connection -- pass `"strength"` explicitly to invert them (`1 /
#'   weight`) so strong ties are treated as short/close rather than costly.
#' @param recalculate Logical. `FALSE` (default) ranks vertices once, on the
#'   intact network (static attack). `TRUE` recomputes betweenness on the
#'   remaining network after every removal (adaptive attack); see
#'   [recalculated_attacks] for details and computational cost.
#'
#' @return A named integer vector in removal order, as produced by
#'   [removal_order()] -- ready to pass to [robustness()].
#'
#' @seealso [removal_order()], [resolve_weights()], [attack_by_degree()],
#'   [attack_by_closeness()], [attack_by_eigenvector()], [attack_by_pagerank()]
#' @export
attack_by_betweenness <- function(g, directed = NULL, weights = NULL, weights_as = NULL,
                                  recalculate = FALSE) {
  if (.check_recalculate(recalculate)) {
    static <- function(gi, w) attack_by_betweenness(gi, directed = directed, weights = w,
                                                    weights_as = weights_as)
    return(.adaptive_order(g, static, weights = weights))
  }
  if (is.null(directed)) {
    directed <- is_directed(g)
  } else if (directed && !is_directed(g)) {
    warning("directed = TRUE was requested, but g is an undirected graph; ",
            "directed has no effect on undirected graphs regardless of its value.")
  }
  weights <- resolve_weights(g, weights, weights_as, native = "distance")
  removal_order(g, betweenness(g, directed = directed, weights = weights), decreasing = TRUE)
}
