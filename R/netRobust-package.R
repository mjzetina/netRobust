#' netRobust: Network Robustness Under Random Failures and Targeted Attacks
#'
#' Simulates and compares network robustness to random node failures and
#' targeted attacks (by degree, betweenness, closeness, eigenvector
#' centrality, PageRank, and Kleinberg's HITS hub/authority scores),
#' extending the approach of Albert, Jeong & Barabasi (2000). Includes a
#' Monte Carlo framework for testing whether an attack scenario is
#' statistically more damaging than random failure, and utilities for
#' plotting robustness curves with a null-model reference envelope.
#'
#' @keywords internal
#' @import igraph
#' @importFrom utils globalVariables
"_PACKAGE"

# Column names referenced unquoted inside dplyr/ggplot2 calls (tidy
# evaluation in summarise_null() and plot_robustness()) -- not real global
# variables, just how NSE column references read to a static check.
globalVariables(c("Index", "N", "Scenario", "Value", "lower", "upper"))
