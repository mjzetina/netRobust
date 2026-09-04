#### EDGE PREP / EDGE REMOVAL ORDER (helpers) ####

# --- Prepare a graph for edge-removal functions (internal) -----------------
# Shared by random_edge_failures(), every attack_by_edge_*() function, and
# edge_robustness() -- called independently by each on the same original g,
# so it must be deterministic (no randomness) for their outputs to stay
# consistent with each other, the same way robustness()'s own
# V(g)$name-fallback naming independently matches whatever attack_by_*()
# already computed.
#
# Two jobs:
#  1. Resolve parallel edges (multiple edges between the same vertex pair),
#     which would break by-name edge identification below. "collapse"
#     (default) merges them via igraph::simplify(), summing weights if any
#     -- same sum-on-collapse convention already used for the
#     directed-to-undirected collapse in mean_topological_diversity
#     (optional_robustness_indicators()). "error" stops instead, for callers
#     who'd rather resolve it themselves than have it decided for them.
#  2. Assign a synthetic, stable edge name ("tail|head") so edges can be
#     deleted by name across the removal loop -- the edge equivalent of how
#     robustness() already deletes vertices by V(g)$name, not by index,
#     because igraph re-indexes after every deletion.
#
# Never mutates the caller's graph in place -- g is a local copy throughout.
.prepare_edge_graph <- function(g, on_parallel = c("collapse", "error")) {
  on_parallel <- match.arg(on_parallel)
  if (is.null(V(g)$name)) V(g)$name <- paste0("node_", seq_len(vcount(g)))

  if (any(which_multiple(g))) {
    n_dup <- sum(which_multiple(g))
    if (on_parallel == "error") {
      stop(n_dup, " edge(s) are redundant members of a parallel-edge group ",
           "(multiple edges between the same pair of vertices), which ",
           "breaks the by-name edge identification this function relies ",
           "on. Pass on_parallel = \"collapse\" (the default for the ",
           "exported functions that call this) to merge them instead of ",
           "erroring.", call. = FALSE)
    }
    warning(n_dup, " redundant edge(s) (parallel edges between the same ",
            "pair of vertices) were collapsed into one, summing their ",
            "weights (if any). Pass on_parallel = \"error\" instead if ",
            "you'd rather this stop and let you resolve it yourself.",
            call. = FALSE)
    comb <- if (is_weighted(g)) list(weight = "sum", "first") else "first"
    g <- simplify(g, remove.multiple = TRUE, remove.loops = FALSE, edge.attr.comb = comb)
  }

  ep <- ends(g, E(g), names = TRUE)
  E(g)$name <- paste(ep[, 1], ep[, 2], sep = "|")
  g
}

# --- Build an edge removal order from any centrality/property vector -------
#' Build an edge removal order from any centrality/property vector
#'
#' The edge-level counterpart of [removal_order()]: shared by
#' [random_edge_failures()] and every `attack_by_edge_*()` function so that
#' adding a new edge-attack criterion doesn't duplicate name-handling logic.
#' Converts a plain numeric vector of edge values into a named, ordered
#' removal sequence usable by [edge_robustness()].
#'
#' @param g An `igraph` object -- must already have gone through
#'   `.prepare_edge_graph()` (i.e. `E(g)$name` set to the stable `"tail|
#'   head"` identifier), which every exported edge function does internally.
#' @param values Numeric vector, one value per edge, in edge-index order
#'   (e.g. `E(g)$weight`, `edge_betweenness(g)`).
#' @param decreasing Logical. `TRUE` (default) removes the highest-value
#'   edge first.
#'
#' @return A named integer vector: values are edge indices (in `g` as
#'   passed in), names are the corresponding `E(g)$name` identifiers, both
#'   reordered into removal order (first element = first edge to remove).
#'
#' @seealso [edge_robustness()], [random_edge_failures()],
#'   [attack_by_edge_betweenness()]
#' @keywords internal
edge_removal_order <- function(g, values, decreasing = TRUE) {
  nm <- E(g)$name
  ord <- order(values, decreasing = decreasing)
  ids <- seq_along(values)[ord]
  names(ids) <- nm[ord]
  ids
}
