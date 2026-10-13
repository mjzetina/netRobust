#### ATTACK BY CLOSENESS ####
#' Targeted-attack removal order by closeness centrality
#'
#' Vertices are removed in decreasing order of closeness centrality --
#' nodes with the shortest average distance to every other node -- first.
#'
#' @param g An `igraph` object.
#' @param directed For API symmetry with [attack_by_betweenness()],
#'   [attack_by_eigenvector()] and [attack_by_pagerank()] (which take a
#'   simple `TRUE`/`FALSE`). `closeness()` doesn't have a single
#'   directed switch -- it's controlled by `mode` below. `directed = NULL`
#'   (default) leaves `mode` untouched. `directed = FALSE` forces
#'   `mode = "all"` regardless of what `mode` was set to, guaranteeing the
#'   same "treat as undirected" behaviour you'd get from `directed = FALSE`
#'   on the other `attack_by_*()` functions -- and warns if that silently
#'   discards an explicit `mode = "out"`/`"in"` you passed. `directed = TRUE`
#'   combined with the default `mode = "all"` has no effect -- `mode =
#'   "all"` ignores edge direction regardless -- and triggers a warning;
#'   pick `mode = "out"` or `mode = "in"` explicitly if you want `directed =
#'   TRUE` to actually have an effect. `directed = TRUE` on a graph that
#'   isn't actually directed (`is_directed(g) == FALSE`) also warns, since
#'   `mode` has no effect on undirected graphs regardless of its value.
#' @param mode One of `"all"` (default), `"out"`, or `"in"`. Only relevant
#'   for directed graphs (ignored otherwise). `mode = "all"` lets shortest
#'   paths traverse edges in either direction -- the practical equivalent
#'   of treating the graph as undirected for distance purposes, which is
#'   why it's the default here even though there's no literal `directed`
#'   argument in `closeness()` itself.
#' @param weights Optional numeric vector of edge weights. If `NULL`
#'   (default) and `g` is weighted, uses `weights_as` to decide how to
#'   interpret `E(g)$weight`; pass a vector explicitly to override both.
#' @param weights_as `NULL` (default) uses closeness's own native weight
#'   interpretation, `"distance"` (like [attack_by_betweenness()], and
#'   unlike [attack_by_eigenvector()]/[attack_by_pagerank()], since
#'   `closeness()` is a shortest-path-based measure): lower weight means a
#'   shorter, more traversable path, weights used as-is. Pass `"strength"`
#'   explicitly to invert them (`1 / weight`) instead.
#' @param recalculate Logical. `FALSE` (default) ranks vertices once, on the
#'   intact network (static attack). `TRUE` recomputes closeness on the
#'   remaining network after every removal (adaptive attack); see
#'   [recalculated_attacks] for details and computational cost.
#'
#' @details
#' `closeness()` normalises *within each connected component*: instead of
#' dividing by the global `n - 1`, it divides each vertex's raw closeness
#' by `(size of its own component) - 1`. If the graph is fully connected
#' this reduces to a single global constant and changes nothing (same as
#' betweenness). But if the input graph `g` is already fragmented -- e.g. an
#' isolated species in a diet matrix -- normalisation stops being a single
#' constant and actually changes the *relative* ranking between vertices in
#' differently-sized components, so it matters here. (By default the
#' removal order is fixed once on the full input graph, per Albert et al.
#' 2000; with `recalculate = TRUE` closeness is recomputed on the remaining,
#' increasingly fragmented network at every step, so this component-wise
#' normalisation applies at every step too.) igraph also emits a warning
#' whenever it has to do this component-wise calculation, which would flood
#' the console if `g` is disconnected and this is called across thousands of
#' Monte Carlo replicates (see [robustness_null_removal()]) -- that warning is
#' intentionally suppressed.
#'
#' @return A named integer vector in removal order, as produced by
#'   [removal_order()] -- ready to pass to [robustness()].
#'
#' @seealso [removal_order()], [attack_by_degree()], [attack_by_betweenness()],
#'   [attack_by_eigenvector()], [attack_by_pagerank()]
#' @export
attack_by_closeness <- function(g, directed = NULL, mode = c("all", "out", "in"),
                                 weights = NULL, weights_as = NULL, recalculate = FALSE) {
  mode_missing <- missing(mode)
  if (.check_recalculate(recalculate)) {
    static <- if (mode_missing) {
      function(gi, w) attack_by_closeness(gi, directed = directed, weights = w,
                                          weights_as = weights_as)
    } else {
      function(gi, w) attack_by_closeness(gi, directed = directed, mode = mode, weights = w,
                                          weights_as = weights_as)
    }
    return(.adaptive_order(g, static, weights = weights))
  }
  mode <- match.arg(mode)
  if (!is.null(directed)) {
    if (directed && !is_directed(g)) {
      warning("directed = TRUE was requested, but g is an undirected graph; ",
              "mode has no effect on undirected graphs regardless of its value.")
    } else if (!directed) {
      if (!mode_missing && mode != "all") {
        warning("directed = FALSE overrides mode = \"", mode, "\"; using ",
                "mode = \"all\" instead (directed = FALSE always wins).")
      }
      mode <- "all"  # directed=FALSE always wins, matches sibling functions
    } else if (mode == "all") {
      warning("directed = TRUE has no effect here: mode = \"all\" (the default) ",
              "lets shortest paths traverse edges in either direction in ",
              "closeness(), which is equivalent to treating the graph as ",
              "undirected for distance purposes. Set mode = \"out\" or ",
              "mode = \"in\" explicitly for directed = TRUE to actually have ",
              "an effect.")
    }
  }
  weights <- resolve_weights(g, weights, weights_as, native = "distance")
  cl <- suppressWarnings(closeness(g, mode = mode, weights = weights, normalized = TRUE))
  removal_order(g, cl, decreasing = TRUE)
}
