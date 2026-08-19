#### ATTACK BY DEGREE ####
#' Targeted-attack removal order by degree centrality
#'
#' Generates the classic "attack" scenario of Albert et al. (2000): vertices
#' are removed in decreasing order of degree, highest-degree (hub) nodes
#' first.
#'
#' @param g An `igraph` object.
#' @param directed Should edge direction be considered? `NULL` (default)
#'   auto-detects from `g` via `is_directed()`. `directed = FALSE` forces
#'   `mode = "all"` regardless of what `mode` was set to (same
#'   "treat as undirected" override used by [attack_by_closeness()] and
#'   the other `attack_by_*()` functions) -- and warns if that silently
#'   discards an explicit `mode = "out"`/`"in"` you passed. `directed = TRUE`
#'   combined with the default `mode = "all"` has no effect -- `mode =
#'   "all"` sums in- and out-degree regardless -- and triggers a warning;
#'   pick `mode = "out"` or `mode = "in"` explicitly if you want `directed =
#'   TRUE` to actually have an effect. `directed = TRUE` on a graph that
#'   isn't actually directed (`is_directed(g) == FALSE`) also warns, since
#'   `mode` has no effect at all on undirected graphs regardless of its
#'   value.
#' @param mode One of `"all"` (default), `"out"`, or `"in"`. Only relevant
#'   for directed graphs (ignored otherwise): `"out"` ranks by outgoing
#'   connections (e.g. how many prey a predator consumes), `"in"` by
#'   incoming connections (e.g. how many predators a species feeds), `"all"`
#'   sums both.
#'
#' @return A named integer vector in removal order, as produced by
#'   [removal_order()] -- ready to pass to [robustness()].
#'
#' @seealso [removal_order()], [attack_by_betweenness()],
#'   [attack_by_closeness()], [attack_by_eigenvector()],
#'   [attack_by_pagerank()]
#' @export
attack_by_degree <- function(g, directed = NULL, mode = c("all", "out", "in")) {
  mode_missing <- missing(mode)
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
              "sums in- and out-degree, which is equivalent to treating the graph ",
              "as undirected. Set mode = \"out\" or mode = \"in\" explicitly for ",
              "directed = TRUE to actually have an effect.")
    }
  }
  removal_order(g, degree(g, mode = mode), decreasing = TRUE)
}
