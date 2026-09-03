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
#' @param index Character vector of indicator column name(s) to summarise.
#'   `NULL` (default) uses every indicator column present in `null_df`.
#'   More than one facets via `ggplot2::facet_wrap(~Index, scales =
#'   "free_x")` (R-indices aren't on comparable scales), the same pattern
#'   as [plot_shapley()].
#' @param bins Number of histogram bins per panel. Default `30`.
#'
#' @return A `ggplot` object (add further layers/themes with `+` as usual).
#'
#' @seealso [compare_robustness_index_to_null()], [robustness_index_null()],
#'   [robustness_index()]
#' @export
plot_r_index_null <- function(null_df, observed = NULL, index = NULL, bins = 30) {
  mat <- .robustness_index_per_replicate(null_df, index = index)
  index <- colnames(mat)  # NULL resolved to every indicator present, inside the helper above
  df <- do.call(rbind, lapply(index, function(ix) {
    data.frame(Index = ix, R_index = mat[, ix])
  }))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$R_index)) +
    ggplot2::geom_histogram(bins = bins, fill = "grey70", colour = "white")

  if (length(index) > 1) {
    p <- p + ggplot2::facet_wrap(~Index, scales = "free_x")
  }

  if (!is.null(observed)) {
    obs_df <- data.frame(
      Index = index,
      R_index = vapply(index, function(ix) robustness_index(observed, index = ix), numeric(1))
    )
    p <- p + ggplot2::geom_vline(
      data = obs_df, ggplot2::aes(xintercept = .data$R_index),
      colour = "#D62728", linewidth = 1, linetype = "dashed"
    )
  }

  p +
    ggplot2::labs(
      x = if (length(index) == 1) paste0("R-index (", index, ") across null replicates")
          else "R-index across null replicates",
      y = "Count",
      caption = if (!is.null(observed)) "Dashed line: observed scenario's R-index" else NULL
    ) +
    ggplot2::theme_bw()
}
