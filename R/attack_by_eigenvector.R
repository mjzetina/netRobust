#### ATTACK BY EIGENVECTOR ####
#' Targeted-attack removal order by eigenvector centrality
#'
#' Vertices are removed in decreasing order of eigenvector centrality --
#' nodes connected to other well-connected nodes ("influence by
#' association") -- first.
#'
#' @param g An `igraph` object.
#' @param directed Should edge direction be considered? `NULL` (default)
#'   auto-detects from `g` via `is_directed()`. Only override to
#'   deliberately treat a directed graph as undirected.
#' @param weights Optional numeric vector of edge weights. If `NULL`
#'   (default) and `g` is weighted, uses `weights_as` to decide how to
#'   interpret `E(g)$weight`; pass a vector explicitly to override both.
#' @param weights_as `NULL` (default) uses eigenvector centrality's own
#'   native weight interpretation, `"strength"` -- higher weight means more
#'   influence transmitted, weights used as-is (matches most weighted
#'   network data directly, e.g. a diet fraction, a tie strength), same as
#'   [attack_by_pagerank()]. Pass `"distance"` explicitly to invert them
#'   (`1 / weight`) instead, e.g. to explore the opposite interpretation of
#'   the same data, mirroring the choice offered in
#'   [attack_by_betweenness()]. Ignored on unweighted graphs.
#' @param recalculate Logical. `FALSE` (default) ranks vertices once, on the
#'   intact network (static attack). `TRUE` recomputes eigenvector centrality on the
#'   remaining network after every removal (adaptive attack); see
#'   [recalculated_attacks] for details and computational cost.
#'
#' @details
#' Eigenvector centrality relies on the Perron-Frobenius theorem, which
#' guarantees a well-behaved (unique, positive) solution on
#' strongly-connected graphs. On directed graphs that aren't strongly
#' connected -- common in real trophic networks (sink species with no
#' outgoing edges) or hierarchical governance networks without feedback
#' loops -- the computation can fail to converge. When `eigen_centrality()`
#' throws a hard error, this function falls back to
#' [attack_by_pagerank()] automatically, with a warning. Note this only
#' catches hard errors: a problematic directed graph can also produce a
#' silently degenerate result (e.g. near-zero for all but one vertex)
#' without erroring at all. For directed networks, consider using
#' [attack_by_pagerank()] directly rather than relying on this fallback.
#'
#' @return A named integer vector in removal order, as produced by
#'   [removal_order()] -- ready to pass to [robustness()].
#'
#' @seealso [removal_order()], [attack_by_degree()], [attack_by_betweenness()],
#'   [attack_by_closeness()], [attack_by_pagerank()]
#' @export
attack_by_eigenvector <- function(g, directed = NULL, weights = NULL, weights_as = NULL,
                                  recalculate = FALSE) {
  if (.check_recalculate(recalculate)) {
    static <- function(gi, w) attack_by_eigenvector(gi, directed = directed, weights = w,
                                                    weights_as = weights_as)
    return(.adaptive_order(g, static, weights = weights))
  }
  if (is.null(directed)) {
    directed <- is_directed(g)
  } else if (directed && !is_directed(g)) {
    warning("directed = TRUE was requested, but g is an undirected graph; ",
            "directed has no effect on undirected graphs regardless of its value.")
  }
  weights <- resolve_weights(g, weights, weights_as, native = "strength")
  ec <- tryCatch(
    eigen_centrality(g, directed = directed, weights = weights)$vector,
    error = function(e) {
      warning("eigen_centrality() failed (common on directed/disconnected graphs); ",
              "falling back to attack_by_pagerank().")
      page_rank(g, directed = directed, weights = weights)$vector
    }
  )
  removal_order(g, ec, decreasing = TRUE)
}
