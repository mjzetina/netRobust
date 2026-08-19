#### REMOVAL ORDER (helper) ####
#' Build a vertex removal order from any centrality/property vector
#'
#' Shared by [random_failures()] and every `attack_by_*()` function, so that
#' adding a new attack criterion (closeness, eigenvector, PageRank, ...) is a
#' one-line wrapper instead of duplicated name-handling logic. Converts a
#' plain numeric vector of vertex values into a named, ordered removal
#' sequence usable by [robustness()].
#'
#' @param g An `igraph` object. Only used to recover vertex names when
#'   `values` doesn't already have them.
#' @param values Numeric vector, one value per vertex, in vertex-index
#'   order (e.g. `degree(g)`, `betweenness(g)`). May be named or unnamed.
#' @param decreasing Logical. `TRUE` (default) removes the highest-value
#'   vertex first (an attack); pass a vector of random values with
#'   `decreasing = TRUE` to get a uniformly random removal order (see
#'   [random_failures()]).
#'
#' @return A named integer vector: values are the original vertex indices,
#'   names are the corresponding vertex names, both reordered into removal
#'   order (first element = first vertex to remove).
#'
#' @seealso [robustness()], [random_failures()], [attack_by_degree()]
#' @keywords internal
removal_order <- function(g, values, decreasing = TRUE) {
  nm <- names(values)
  if (is.null(nm)) nm <- V(g)$name
  if (is.null(nm)) nm <- paste0("node_", seq_len(vcount(g)))

  ord <- order(values, decreasing = decreasing)
  ids <- seq_along(values)[ord]
  names(ids) <- nm[ord]
  ids
}
