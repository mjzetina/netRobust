#### EDGE ROBUSTNESS ####
#' Network robustness under sequential edge removal
#'
#' The edge-level counterpart of [robustness()]: removes edges from `g` one
#' at a time, in the order given by `ids` (typically the output of
#' [random_edge_failures()] or an `attack_by_edge_*()` function),
#' recomputing the same structural indicators after each removal. All
#' vertices/actors stay in the network throughout -- only specific
#' relationships/ties disappear, so this answers "how robust is the network
#' to losing relationships without losing the actors themselves?", as
#' opposed to [robustness()]'s "...to losing actors?".
#'
#' Every indicator, and every one of `indicators=`/`exclude=`/`keep=`/
#' `custom=`/`optional=`/`directed=`/`component_mode=`/`mode=`/`weights=`/
#' `weights_as=`, works identically to [robustness()] -- they're properties
#' of the network and how to measure it, not specific to whether a vertex
#' or an edge is being removed. See `?robustness` for the full description
#' of each.
#'
#' @param g An `igraph` object to analyze.
#' @param ids Named removal vector (output of [random_edge_failures()] or
#'   an `attack_by_edge_*()` function). `names(ids)` must be the `"tail|
#'   head"` edge identifiers `.prepare_edge_graph()` assigns internally --
#'   you never construct these by hand, they come from whichever function
#'   built `ids`.
#' @param indicators,exclude,keep,custom,optional,directed,component_mode,mode,weights,weights_as
#'   As in [robustness()].
#' @param on_parallel How to handle parallel edges (multiple edges between
#'   the same pair of vertices), which would otherwise break the by-name
#'   edge identification this function relies on. `"collapse"` (default)
#'   merges them via `igraph::simplify()`, summing weights if any, with a
#'   warning -- must match whichever `on_parallel` was used to build `ids`
#'   (both default to `"collapse"`, so this is transparent unless you
#'   overrode one of them). `"error"` stops instead. See
#'   `.prepare_edge_graph()`.
#'
#' @return A `data.frame` with one row per network state, from the intact
#'   network (`n_removed = 0`) to the one with every edge removed
#'   (`n_removed = ecount(g)` after any parallel-edge collapse), and one
#'   column per indicator -- same shape as
#'   [robustness()]'s output, so every downstream function that consumes it
#'   ([compare_to_null()], [robustness_index()],
#'   [compare_robustness_index_to_null()], [plot_robustness()],
#'   [plot_r_index_null()]...) works on edge-removal curves with no changes.
#'
#' @section edge_density is order-invariant here:
#' Under edge removal, `edge_density` (edges / possible edges) depends only on
#' *how many* edges have been removed, never on *which* ones -- the vertex
#' set (and so the denominator) never changes, and every removed edge drops
#' the numerator by exactly 1 regardless of which edge it was. Its curve is
#' therefore mathematically identical across every edge-removal scenario on
#' the same starting graph (random failure, any `attack_by_edge_*()`
#' criterion) -- not similar, identical. This is specific to *edge*
#' removal: under [robustness()]'s node removal, `edge_density`'s denominator
#' shrinks with the vertex count and the numerator's drop per step depends
#' on the removed node's degree, so it *is* scenario-sensitive there. No
#' other indicator shares this order-invariance for edge removal -- giant_component_size,
#' mean_distance, diameter, connectivity, degree_centralization, clustering, degree_assortativity,
#' reciprocity, and mean_topological_diversity all depend on which edges
#' were removed, not just how many. Comparing `edge_density` statistically
#' between edge-removal scenarios (`compare_to_null()`,
#' `compare_robustness_index_to_null()`) has no power here -- it can never
#' show a difference, by construction, regardless of your data. Consider
#' `exclude = "edge_density"` for that kind of comparison.
#'
#' @seealso [robustness()], [random_edge_failures()],
#'   [attack_by_edge_betweenness()], [attack_by_edge_weight()],
#'   [edge_robustness_null_removal()], [list_indicators()]
#' @export
edge_robustness <- function(g, ids, indicators = NULL, exclude = NULL, keep = NULL,
                             custom = NULL, optional = NULL,
                             directed = NULL, component_mode = "weak", mode = NULL,
                             weights = NULL, weights_as = NULL,
                             on_parallel = c("collapse", "error")) {
  g <- .prepare_edge_graph(g, on_parallel = on_parallel)

  setup <- .resolve_robustness_setup(g, indicators, exclude, keep, custom, optional,
                                      directed, component_mode, mode, weights, weights_as)
  g <- setup$g

  n <- ecount(g)
  removal_names <- names(ids)[seq_len(n)]
  stopifnot(all(removal_names %in% E(g)$name))

  # Delete one edge at a time from the already-shrunk graph, by NAME (same
  # rationale as robustness() deleting vertices by name -- see
  # .removal_curve()).
  .removal_curve(g, removal_names, setup$indicators, delete_edges)
}
