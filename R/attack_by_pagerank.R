#### ATTACK BY PAGERANK ####
#' Targeted-attack removal order by PageRank score
#'
#' Vertices are removed in decreasing order of PageRank -- especially
#' recommended for directed, weighted networks (trophic webs, value chains,
#' governance networks): handles cycles and sink nodes well, where
#' eigenvector centrality can struggle (see [attack_by_eigenvector()]).
#'
#' @param g An `igraph` object.
#' @param directed Should edge direction be considered? `NULL` (default)
#'   auto-detects from `g` via `is_directed()`. Pass `FALSE` to
#'   deliberately treat a directed graph as undirected.
#' @param weights Optional numeric vector of edge weights. If `NULL`
#'   (default) and `g` is weighted, uses `weights_as` to decide how to
#'   interpret `E(g)$weight`; pass a vector explicitly to override both.
#' @param weights_as `NULL` (default) uses PageRank's own native weight
#'   interpretation, `"strength"` -- higher weight means more influence
#'   transmitted, weights used as-is (matches most weighted network data
#'   directly). Pass `"distance"` explicitly to invert them (`1 / weight`)
#'   instead, to explore the opposite interpretation, mirroring the choice
#'   in [attack_by_betweenness()] and [attack_by_eigenvector()].
#'
#' @return A named integer vector in removal order, as produced by
#'   [removal_order()] -- ready to pass to [robustness()].
#'
#' @seealso [removal_order()], [attack_by_degree()], [attack_by_betweenness()],
#'   [attack_by_closeness()], [attack_by_eigenvector()]
#' @export
attack_by_pagerank <- function(g, directed = NULL, weights = NULL, weights_as = NULL) {
  if (is.null(directed)) {
    directed <- is_directed(g)
  } else if (directed && !is_directed(g)) {
    warning("directed = TRUE was requested, but g is an undirected graph; ",
            "directed has no effect on undirected graphs regardless of its value.")
  }
  weights <- resolve_weights(g, weights, weights_as, native = "strength")
  pr <- page_rank(g, directed = directed, weights = weights)$vector
  removal_order(g, pr, decreasing = TRUE)
}
