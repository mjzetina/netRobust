#### RECALCULATED (ADAPTIVE) ATTACKS ####
#' Recalculated (adaptive) targeted attacks
#'
#' By default every `attack_by_*()` function computes its centrality once,
#' on the intact network, and returns that fixed ("static") removal order,
#' as in Albert et al. (2000). With `recalculate = TRUE` the centrality is
#' instead recomputed on the remaining network after every removal: the
#' current top-ranked vertex (or edge) is removed, the measure is computed
#' again on what is left, and so on until the network is empty. This is the
#' "recalculated" attack of Holme et al. (2002), also used by
#' NetworkExtinction (`Mostconnected`), `bipartite::second.extinct(method =
#' "degree")` and NetSwan's cascading attack. It is usually at least as
#' damaging as the static attack, because it keeps targeting whatever has
#' become central after earlier losses.
#'
#' @details
#' **Same output format.** The result is the same kind of named integer
#' vector as the static attack (names = vertex or edge names, values =
#' indices in the *original* graph), so it can be passed unchanged to
#' [robustness()], [edge_robustness()] or used inside the `attack_fn` of
#' [robustness_null_structural()], e.g.
#' `attack_fn = function(g) attack_by_degree(g, recalculate = TRUE)`.
#'
#' **Same options at every step.** Each step calls the static attack on the
#' current graph with the same arguments (`directed`, `mode`, `weights`,
#' `weights_as`, `which`, ...). An explicit `weights` vector is matched to
#' its edges, so it shrinks with the network as edges disappear. Ties are
#' broken exactly as in the static attack (the lower original index first;
#' see [attack_by_hits()] for its own tie-break).
#'
#' **HITS and reproducibility.** As the network shrinks, a recalculated HITS
#' attack often reaches small subgraphs (e.g. a chain A -> B -> C) whose
#' leading eigenvalue is repeated. There the scores are not unique, and
#' ARPACK's random starting vector decides which solution is returned, so
#' the removal order can change between runs even though ties are broken
#' deterministically. Call `set.seed()` before the attack for a
#' reproducible order. Degree, betweenness, closeness, eigenvector and
#' PageRank attacks do not depend on the random seed.
#'
#' **No edges left.** Once the remaining vertices have no edges, every
#' centrality is zero or undefined, so the rest are appended in original
#' index order without further computation.
#'
#' **Warnings** raised while recomputing (e.g. the eigenvector-to-PageRank
#' fallback, or an argument that has no effect) are collected and issued
#' once each, not once per step.
#'
#' **Cost.** A recalculated attack computes the centrality up to *n* times
#' instead of once. For degree this is cheap; for betweenness and closeness
#' (each roughly O(n m) per computation on a graph with *n* vertices and *m*
#' edges) the whole attack is roughly O(n^2 m), and inside
#' [robustness_null_structural()] it is repeated for every replicate. Time a
#' single call on your network before running large null models.
#'
#' `attack_by_edge_weight()` has no `recalculate` option: an edge's weight
#' does not change when other edges are removed, so recalculating would
#' always reproduce the static order.
#'
#' @references
#' Albert, R., Jeong, H. & Barabasi, A.-L. (2000). Error and attack
#' tolerance of complex networks. *Nature*, 406, 378--382.
#'
#' Holme, P., Kim, B. J., Yoon, C. N. & Han, S. K. (2002). Attack
#' vulnerability of complex networks. *Physical Review E*, 65, 056109.
#'
#' @seealso [attack_by_degree()], [attack_by_betweenness()],
#'   [attack_by_closeness()], [attack_by_eigenvector()],
#'   [attack_by_pagerank()], [attack_by_hits()],
#'   [attack_by_edge_betweenness()]
#' @name recalculated_attacks
NULL

# --- Build a recalculated removal order (internal) --------------------------
# static_fn(gi, w) must return the static removal order of the current graph
# gi (indices into gi, as removal_order()/edge_removal_order() produce), given
# the edge weights w that belong to gi's remaining edges (NULL or NA passed
# through unchanged). Each step removes static_fn()'s first element, tracking
# original indices through a hidden attribute because igraph re-indexes
# after every deletion. For type = "edge", g must already have been through
# .prepare_edge_graph().
.adaptive_order <- function(g, static_fn, type = c("vertex", "edge"), weights = NULL) {
  type <- match.arg(type)
  if (type == "vertex") {
    n  <- vcount(g)
    nm <- V(g)$name
    if (is.null(nm)) nm <- paste0("node_", seq_len(n))
    g  <- set_vertex_attr(g, ".nr_id", value = seq_len(n))
  } else {
    n  <- ecount(g)
    nm <- E(g)$name
    g  <- set_edge_attr(g, ".nr_id", value = seq_len(n))
  }
  explicit_w <- !is.null(weights) && !(length(weights) == 1 && is.na(weights))
  if (explicit_w) {
    if (length(weights) != ecount(g)) {
      stop("weights must have one value per edge: length(weights) = ", length(weights),
           ", but ecount(g) = ", ecount(g), ".", call. = FALSE)
    }
    g <- set_edge_attr(g, ".nr_w", value = weights)
  }

  out   <- integer(0)
  warns <- character(0)
  gi    <- g
  while (length(out) < n) {
    if (type == "vertex" && ecount(gi) == 0) {
      out <- c(out, sort(V(gi)$.nr_id))  # nothing left to rank: index order
      break
    }
    w <- if (explicit_w) edge_attr(gi, ".nr_w") else weights
    ord <- withCallingHandlers(
      static_fn(gi, w),
      warning = function(cond) {
        warns <<- c(warns, conditionMessage(cond))
        invokeRestart("muffleWarning")
      }
    )
    first <- ord[[1]]
    if (type == "vertex") {
      out <- c(out, V(gi)$.nr_id[first])
      gi  <- delete_vertices(gi, first)
    } else {
      out <- c(out, E(gi)$.nr_id[first])
      gi  <- delete_edges(gi, first)
    }
  }
  for (msg in unique(warns)) warning(msg, call. = FALSE)
  names(out) <- nm[out]
  out
}

# Shared argument check for the recalculate= switch of every attack_by_*().
.check_recalculate <- function(recalculate) {
  if (!is.logical(recalculate) || length(recalculate) != 1 || is.na(recalculate)) {
    stop("recalculate must be TRUE or FALSE.", call. = FALSE)
  }
  recalculate
}
