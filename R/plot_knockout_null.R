# Network robustness through failures and attack scenarios ####
# Manuel J. Zetina-Rejon
# Plot a knock-out null distribution, faceted by group size, against
# observed group(s)

#' Plot a knock-out null distribution against observed group(s)
#'
#' Histogram of a knock-out null distribution's indicator values (from
#' [knockout_robustness_null_removal()]), faceted by group size, with an
#' optional vertical line per observed group (from [knockout_robustness()])
#' marking where it falls -- the visual counterpart to
#' [compare_knockout_to_null()]'s p-value. A single group size produces a
#' single panel; several sizes produce one panel per size, via
#' `ggplot2::facet_wrap()`, which degrades to one panel on its own when
#' there's only one size -- no special-casing needed here for that.
#'
#' @param null_df Output of [knockout_robustness_null_removal()].
#' @param observed Optional. Output of [knockout_robustness()] -- one or
#'   more groups to mark, each drawn as its own coloured vertical line
#'   (labelled by its exact vertex names in the legend) in the panel
#'   matching its size. `NULL` (default) omits these, e.g. to just inspect
#'   the null's own shape. A group whose size has no matching null
#'   replicates in `null_df` still draws (with no histogram behind it in
#'   that panel) but warns -- this is a visual aid, not a significance
#'   test; use [compare_knockout_to_null()] for that, which errors on the
#'   same situation instead.
#' @param index Name of the indicator column to plot (default
#'   `"giant_component_size"`).
#' @param bins Number of histogram bins per panel. Default `30`.
#'
#' @return A `ggplot` object (add further layers/themes with `+` as usual).
#'
#' @seealso [compare_knockout_to_null()], [knockout_robustness_null_removal()],
#'   [knockout_robustness()], [plot_r_index_null()]
#' @export
plot_knockout_null <- function(null_df, observed = NULL, index = "giant_component_size", bins = 30) {
  if (!index %in% names(null_df)) {
    stop("index = \"", index, "\" not found in null_df. Available columns: ",
         paste(setdiff(names(null_df), c("group_id", "group", "n_removed")), collapse = ", "),
         ".", call. = FALSE)
  }
  null_df$.value <- null_df[[index]]

  p <- ggplot2::ggplot(null_df, ggplot2::aes(x = .data$.value)) +
    ggplot2::geom_histogram(bins = bins, fill = "grey70", colour = "white") +
    ggplot2::facet_wrap(
      ~n_removed, scales = "free",
      labeller = ggplot2::as_labeller(function(x) paste("Group size:", x))
    )

  if (!is.null(observed)) {
    missing_sizes <- setdiff(unique(observed$n_removed), unique(null_df$n_removed))
    if (length(missing_sizes) > 0) {
      warning(length(missing_sizes), " observed group size(s) have no matching ",
              "null replicates in null_df, so those line(s) will show with no ",
              "histogram behind them: ", paste(missing_sizes, collapse = ", "),
              ".", call. = FALSE)
    }
    observed$.value <- observed[[index]]
    observed$.label <- vapply(observed$group, paste, character(1), collapse = ", ")
    p <- p + ggplot2::geom_vline(
      data = observed,
      ggplot2::aes(xintercept = .data$.value, colour = .data$.label),
      linewidth = 1, linetype = "dashed"
    )
  }

  p +
    ggplot2::labs(
      x = paste0(index, " across null replicates"),
      y = "Count",
      colour = "Observed group",
      caption = if (!is.null(observed)) "Dashed lines: observed group(s), faceted by group size" else NULL
    ) +
    ggplot2::theme_bw()
}
