# Network robustness through failures and attack scenarios ####
# Manuel J. Zetina-Rejon
# Plot a null R-index distribution against an observed value

#' Plot a null R-index distribution against an observed value
#'
#' Histogram of the null model's per-replicate R-index values (from
#' [robustness_null_removal()]/[robustness_null_structural()]), with an
#' optional vertical line marking an observed scenario's own R-index -- the
#' visual counterpart to [compare_robustness_index_to_null()]'s p-value.
#'
#' @param null_df Output of [robustness_null_removal()] or
#'   [robustness_null_structural()] (the raw, wide-format replicates --
#'   not [summarise_null()]'s output; passing that by mistake errors with
#'   a specific message pointing at the fix).
#' @param observed Optional. Output of [robustness()] for the scenario to
#'   mark on the plot (e.g. an `attack_by_*()` curve) -- drawn as a
#'   vertical line at its R-index. `NULL` (default) omits the line, e.g.
#'   to just inspect the null distribution's own shape.
#' @param index Name of the indicator column to summarise (default `"giant_component_size"`).
#' @param bins Number of histogram bins. Default `30`.
#'
#' @return A `ggplot` object (add further layers/themes with `+` as usual).
#'
#' @seealso [compare_robustness_index_to_null()], [robustness_index_null()],
#'   [robustness_index()]
#' @export
plot_r_index_null <- function(null_df, observed = NULL, index = "giant_component_size", bins = 30) {
  null_vals <- .robustness_index_per_replicate(null_df, index = index)
  df <- data.frame(R_index = null_vals)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$R_index)) +
    ggplot2::geom_histogram(bins = bins, fill = "grey70", colour = "white")

  if (!is.null(observed)) {
    obs_index <- robustness_index(observed, index = index)
    p <- p + ggplot2::geom_vline(xintercept = obs_index, colour = "#D62728",
                                  linewidth = 1, linetype = "dashed")
  }

  p +
    ggplot2::labs(
      x = paste0("R-index (", index, ") across null replicates"),
      y = "Count",
      caption = if (!is.null(observed)) "Dashed line: observed scenario's R-index" else NULL
    ) +
    ggplot2::theme_bw()
}
