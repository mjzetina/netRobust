# Network robustness through failures and attack scenarios ####
# Manuel J. Zetina-Rejon
# Forest/caterpillar plot of per-node Shapley values, optionally faceted by
# indicator

#' Plot per-node Shapley values, optionally faceted by indicator
#'
#' Forest/caterpillar plot of [shapley_robustness()]'s per-node estimates:
#' one point + error bar per node (mean Shapley value +/- 1.96 standard
#' errors, an approximate 95% CI), ordered so the biggest and smallest
#' contributors stand out at a glance -- the visual counterpart to
#' `shapley_robustness()$global_test`'s p-value, for judging individual
#' nodes without a battery of pairwise tests (see [shapley_robustness()]
#' Details for why that's deliberately not offered).
#'
#' More than one indicator facets via `ggplot2::facet_wrap(~Index, scales =
#' "free_x")` (indicators aren't on comparable scales), the same pattern as
#' [plot_knockout_null()]. Node order is fixed across every panel (see
#' `order=`) so a node's position can be tracked across indicators; each
#' facet strip also shows that indicator's global-test p-value.
#'
#' @section The dashed zero line:
#' Marks zero mean contribution -- a node whose confidence interval crosses
#' it isn't distinguishable from "no effect" at that indicator. Shapley
#' values are positive when the node's removal reduced the indicator, and
#' negative when its removal increased it. Whether *negative* values are
#' possible at all depends on the indicator.
#' `giant_component_size`/`connectivity` can only shrink or stay the same
#' as vertices are removed, so their contributions are always `>= 0`.
#' `mean_distance`/`diameter`, by contrast, often *increase* before a
#' network fragments (remaining paths have to route around whatever was
#' just removed) -- a node with a negative contribution there was, on
#' average, a shortcut whose absence made the rest of the network less
#' efficient to traverse. `clustering`, `degree_assortativity`,
#' `edge_density`, and `degree_centralization` aren't monotonic either and
#' can go either way, without as clean an interpretation.
#'
#' @param x Output of [shapley_robustness()].
#' @param indicators Character vector of indicator names to plot. `NULL`
#'   (default) plots every indicator in `x$nodes`.
#' @param top_n Optional integer: show only the `top_n` nodes with the
#'   largest mean `abs()` value across the plotted indicators (so a node
#'   that stands out strongly in either direction is kept). `NULL`
#'   (default) shows every node -- fine for a Karate-Club-sized network,
#'   but likely too dense for a much larger one.
#' @param order `"value"` (default) orders nodes by their mean (signed)
#'   value across the plotted indicators, so the biggest/smallest
#'   contributors sit at opposite ends -- the point of a forest plot.
#'   `"vertex"` keeps `g`'s original vertex order instead, e.g. to line
#'   this plot up against another output that also uses vertex order.
#' @param null_color Colour for the dashed zero-contribution reference line
#'   -- same role and default (`"grey40"`) as [plot_robustness()]'s
#'   `null_color=`.
#'
#' @return A `ggplot` object.
#'
#' @seealso [shapley_robustness()], [plot_knockout_null()]
#' @export
plot_shapley <- function(x, indicators = NULL, top_n = NULL, order = c("value", "vertex"),
                          null_color = "grey40") {
  order <- match.arg(order)
  if (!inherits(x, "shapley_robustness")) {
    stop("x must be the output of shapley_robustness().", call. = FALSE)
  }
  nodes_df <- x$nodes
  se_cols <- grep("_se$", names(nodes_df), value = TRUE)
  all_indicators <- setdiff(names(nodes_df), c("node", se_cols))
  if (is.null(indicators)) indicators <- all_indicators
  unknown <- setdiff(indicators, all_indicators)
  if (length(unknown) > 0) {
    stop("indicators has name(s) not found in x$nodes: ", paste(unknown, collapse = ", "),
         ". Available: ", paste(all_indicators, collapse = ", "), ".", call. = FALSE)
  }

  long <- do.call(rbind, lapply(indicators, function(nm) {
    data.frame(
      node   = nodes_df$node,
      Index  = nm,
      .value = nodes_df[[nm]],
      .se    = nodes_df[[paste0(nm, "_se")]],
      stringsAsFactors = FALSE
    )
  }))

  # Rank nodes for top_n= (by mean absolute value, so a strongly negative
  # contributor is kept alongside strongly positive ones) and for
  # order = "value" (by mean signed value, for a smooth gradient display).
  val_mat <- do.call(cbind, lapply(indicators, function(nm) nodes_df[[nm]]))
  signed_val <- stats::setNames(rowMeans(val_mat), nodes_df$node)
  abs_val    <- stats::setNames(rowMeans(abs(val_mat)), nodes_df$node)

  if (!is.null(top_n)) {
    keep_nodes <- names(sort(abs_val, decreasing = TRUE))[seq_len(min(top_n, length(abs_val)))]
    long <- long[long$node %in% keep_nodes, ]
    signed_val <- signed_val[keep_nodes]
  }

  node_levels <- if (order == "value") {
    names(sort(signed_val))
  } else {
    nodes_df$node[nodes_df$node %in% unique(long$node)]
  }
  long$node <- factor(long$node, levels = node_levels)

  p_labels <- NULL
  if (!is.null(x$global_test)) {
    gt <- x$global_test[x$global_test$Index %in% indicators, ]
    p_labels <- stats::setNames(
      paste0(gt$Index, " (global p = ", formatC(gt$p_value, digits = 3, format = "f"), ")"),
      gt$Index
    )
  }

  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$.value, y = .data$node)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = null_color) +
    ggplot2::geom_pointrange(
      ggplot2::aes(xmin = .data$.value - 1.96 * .data$.se, xmax = .data$.value + 1.96 * .data$.se)
    )

  if (length(indicators) > 1) {
    p <- p + ggplot2::facet_wrap(
      ~Index, scales = "free_x",
      labeller = if (!is.null(p_labels)) ggplot2::as_labeller(p_labels) else ggplot2::label_value
    )
  } else if (!is.null(p_labels)) {
    p <- p + ggplot2::labs(subtitle = unname(p_labels))
  }

  p +
    ggplot2::labs(
      x = "Shapley value (mean marginal contribution +/- 95% CI)", y = NULL,
      caption = paste("Dashed line: zero mean contribution. Shapley values are positive when",
                       "the node's removal reduced the indicator, and negative when its",
                       "removal increased it.")
    ) +
    ggplot2::theme_bw()
}
