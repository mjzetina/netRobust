#### RESOLVE WEIGHTS (helper) ####
#' Resolve an edge-weight vector for centrality-based attack functions
#'
#' Shared by [attack_by_betweenness()], [attack_by_closeness()],
#' [attack_by_eigenvector()] and [attack_by_pagerank()] so the
#' distance-vs-strength weight interpretation logic lives in one place
#' instead of being duplicated across four files.
#'
#' @param g An `igraph` object.
#' @param weights Optional explicit numeric vector of edge weights. If not
#'   `NULL`, must have length `ecount(g)` (one value per edge) or this
#'   errors immediately with a clear message -- rather than letting a
#'   length mismatch reach igraph's C internals, which raise a much more
#'   cryptic low-level error. When valid, returned as-is (always wins over
#'   `weights_as`/`E(g)$weight`).
#' @param weights_as `NULL` (default) uses each measure's own native weight
#'   interpretation (`native`, below) -- no inversion. Pass `"distance"` or
#'   `"strength"` explicitly to force that interpretation regardless of
#'   `native`. Only meaningful when `g` is weighted and `weights = NULL`;
#'   passing it explicitly on an unweighted graph (with no `weights`
#'   vector supplied either) has no effect and triggers a warning.
#' @param native Which of `"distance"`/`"strength"` this particular
#'   centrality measure treats natively (i.e. uses `E(g)$weight` as-is,
#'   with no inversion). `"distance"` for shortest-path-based measures
#'   (betweenness, closeness); `"strength"` for influence-propagation
#'   measures (eigenvector, PageRank). When `weights_as` doesn't match
#'   `native`, the weights are inverted (`1 / weight`).
#'
#' @return `NULL` (unweighted), the explicit `weights` vector, or a
#'   (possibly inverted) numeric vector derived from `E(g)$weight`.
#' @keywords internal
resolve_weights <- function(g, weights, weights_as = NULL, native = c("distance", "strength")) {
  if (!is.null(weights)) {
    if (length(weights) != ecount(g)) {
      stop("weights must have one value per edge: length(weights) = ", length(weights),
           ", but ecount(g) = ", ecount(g), ".", call. = FALSE)
    }
    return(weights)  # explicit vector always wins
  }
  if (!is_weighted(g)) {
    if (!is.null(weights_as)) {
      warning("weights_as = \"", weights_as[1], "\" has no effect: g is an ",
              "unweighted graph and weights = NULL. Pass an explicit weights ",
              "vector if you want weighting to be applied.")
    }
    return(NULL)
  }
  native <- match.arg(native)
  if (is.null(weights_as)) weights_as <- native  # NULL = use this measure's native interpretation
  weights_as <- tryCatch(
    match.arg(weights_as, choices = c("distance", "strength")),
    error = function(e) {
      stop("weights_as must be \"distance\" or \"strength\" (got \"",
           weights_as[1], "\").", call. = FALSE)
    }
  )
  if (weights_as == native) E(g)$weight else 1 / E(g)$weight
}
