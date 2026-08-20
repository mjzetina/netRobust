#### RANDOM FAILURES ####
#' Random node-removal order (error/failure scenario)
#'
#' Generates a uniformly random vertex removal order, representing the
#' "random failure" scenario of Albert et al. (2000): nodes fail
#' independently of any structural property. Internally reuses
#' [removal_order()] by treating a random permutation as if it were a
#' centrality vector -- this keeps `random_failures()` structurally
#' identical to every `attack_by_*()` function, so [robustness()] and
#' [robustness_null_removal()] don't need to know whether an order came from chance
#' or from a targeted attack criterion.
#'
#' @param g An `igraph` object.
#'
#' @return A named integer vector in removal order, as produced by
#'   [removal_order()] -- ready to pass to [robustness()].
#'
#' @seealso [removal_order()], [robustness_null_removal()] (runs many replicates of
#'   this function to build the statistical null distribution)
#' @export
random_failures <- function(g) {
  removal_order(g, sample(vcount(g)), decreasing = TRUE)
}
