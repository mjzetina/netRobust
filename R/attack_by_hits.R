#### ATTACK BY HITS ####
#' Targeted-attack removal order by Kleinberg's HITS hub/authority scores
#'
#' Vertices are removed in decreasing order of their HITS (Hyperlink-Induced
#' Topic Search; Kleinberg 1999) hub score, authority score, or the product
#' of both. HITS defines the two scores jointly: a good *hub* points to good
#' authorities, and a good *authority* is pointed to by good hubs.
#'
#' @param g A directed `igraph` object.
#' @param which Which score drives the attack: `"hub"` (default),
#'   `"authority"`, or `"product"` (hub x authority, which ranks vertices
#'   identically to the geometric mean `sqrt(hub * authority)`).
#' @param weights Optional numeric vector of edge weights. If `NULL`
#'   (default) and `g` is weighted, uses `weights_as` to decide how to
#'   interpret `E(g)$weight`; pass a vector explicitly to override both, or
#'   `NA` to ignore the weights of a weighted graph (unweighted HITS).
#' @param weights_as `NULL` (default) uses HITS's own native weight
#'   interpretation, `"strength"` -- higher weight means a stronger link,
#'   weights used as-is (e.g. a diet fraction), same as
#'   [attack_by_pagerank()] and [attack_by_eigenvector()]. Pass
#'   `"distance"` explicitly to invert them (`1 / weight`) instead.
#'   Ignored on unweighted graphs.
#'
#' @details
#' **Orientation.** Hub and authority depend on edge direction. With edges
#' pointing prey -> predator (the direction of energy flow), a high *hub*
#' score marks a prey whose predators are important consumers, and a high
#' *authority* score marks a consumer that feeds on important prey. With
#' edges pointing predator -> prey the roles swap: reversing every edge of
#' `g` turns hub scores into authority scores and vice versa, so
#' `attack_by_hits(g, "hub")` equals
#' `attack_by_hits(igraph::reverse_edges(g), "authority")`. On an undirected
#' graph hub and authority both equal eigenvector centrality; this function
#' warns and [attack_by_eigenvector()] is the clearer choice there.
#'
#' **Zero scores and ties.** Food webs are close to acyclic, and on such
#' graphs HITS tends to concentrate its scores on a dominant core, leaving
#' many vertices with a score of exactly zero (e.g. basal species always
#' have authority 0 with prey -> predator edges, and even a top predator can
#' get authority 0 if its prey sit outside that core). The `"product"`
#' score is zero for every vertex that lacks either role. Ties are therefore
#' common, and are broken deterministically: scores are first rounded to 8
#' decimal places relative to their maximum, so ARPACK's numerical noise
#' neither turns an exact zero into a tiny positive value nor splits an
#' exact tie (e.g. two prey with hub 0.5 each), then tied vertices are
#' ordered by the *other*
#' score (authority for `"hub"`, hub for `"authority"`, hub + authority for
#' `"product"`), then by total degree, then by vertex index.
#'
#' **Reproducibility.** `igraph::hits_scores()` uses ARPACK, whose iteration
#' starts from a random vector, so scores can differ in the last digits
#' between runs, and more than that when the leading eigenvalue is
#' (nearly) repeated. Call `set.seed()` beforehand for reproducible scores;
#' the rounding and tie-break above keep the removal order stable in the
#' common case of exact ties and zeros.
#'
#' **Caveat.** HITS was designed for ranking web pages. No published
#' application of HITS to food webs or other ecological networks was found
#' (searches up to October 2026); the closest prior work is the
#' PageRank-based ranking of species importance of Allesina & Pascual
#' (2009), see [attack_by_pagerank()]. Treat this attack as exploratory.
#'
#' @return A named integer vector in removal order, as produced by
#'   [removal_order()] -- ready to pass to [robustness()].
#'
#' @references
#' Kleinberg, J. M. (1999). Authoritative sources in a hyperlinked
#' environment. *Journal of the ACM*, 46(5), 604--632.
#'
#' Allesina, S. & Pascual, M. (2009). Googling food webs: can an eigenvector
#' measure species' importance for coextinctions? *PLoS Computational
#' Biology*, 5(9), e1000494.
#'
#' @examples
#' # Toy food web, edges prey -> predator
#' g <- igraph::make_graph(~ A -+ C, B -+ C, C -+ D, B -+ D, D -+ E)
#' set.seed(1)
#' attack_by_hits(g, "hub")        # prey of important consumers first
#' attack_by_hits(g, "authority")  # consumers of important prey first
#'
#' @seealso [removal_order()], [resolve_weights()], [attack_by_pagerank()],
#'   [attack_by_eigenvector()]
#' @export
attack_by_hits <- function(g, which = c("hub", "authority", "product"),
                           weights = NULL, weights_as = NULL) {
  which <- match.arg(which)
  if (!is_directed(g)) {
    warning("g is undirected: hub and authority scores both equal eigenvector ",
            "centrality there; consider attack_by_eigenvector() instead.")
  }
  # weights = NA means "ignore E(g)$weight"; anything else goes through
  # resolve_weights(). Never hand igraph a NULL: hits_scores() would then
  # silently use E(g)$weight.
  if (length(weights) == 1 && is.na(weights)) {
    weights <- NA
  } else {
    weights <- resolve_weights(g, weights, weights_as, native = "strength")
    if (is.null(weights)) weights <- NA
  }

  s <- hits_scores(g, weights = weights)
  # Round relative to the maximum so ARPACK noise in the last digits can't
  # break exact ties or turn exact zeros into tiny positive values.
  denoise <- function(x) {
    m <- max(abs(x))
    if (m == 0) x else round(x / m, 8) * m
  }
  hub <- denoise(s$hub)
  auth <- denoise(s$authority)

  score <- switch(which, hub = hub, authority = auth, product = denoise(hub * auth))
  tie   <- switch(which, hub = auth, authority = hub, product = hub + auth)
  deg   <- degree(g, mode = "all")

  # Rank on (score, tie, degree) descending, vertex index ascending, then
  # hand removal_order() that rank so names are handled in one place.
  rk <- order(order(-score, -tie, -deg, seq_along(score)))
  names(rk) <- names(s$hub)
  removal_order(g, rk, decreasing = FALSE)
}
