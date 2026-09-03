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
#' there's only one size -- no special-casing needed here for that. More
#' than one indicator adds a second facet dimension instead
#' (`ggplot2::facet_grid(Index ~ n_removed)`), so each row is one indicator
#' and each column is one group size.
#'
#' @param null_df Output of [knockout_robustness_null_removal()].
#' @param observed Optional. Output of [knockout_robustness()] -- one or
#'   more groups to mark, each drawn as its own coloured vertical line
#'   (labelled by its exact vertex names in the legend) in the panel
#'   matching its size. `NULL` (default) omits these, e.g. to just inspect
#'   the null's own shape. Its intact-network reference row, if present
#'   (`include_intact = TRUE`, the default), is dropped automatically with
#'   a warning -- there's no null of size 0 to place it against. A group
#'   whose size has no matching null replicates in `null_df` still draws
#'   (with no histogram behind it in that panel) but warns -- this is a
#'   visual aid, not a significance test; use [compare_knockout_to_null()]
#'   for that, which errors on the same situation instead.
#' @param index Character vector of indicator column name(s) to plot.
#'   `NULL` (default) uses every indicator column present in `null_df`.
#' @param bins Number of histogram bins per panel. Default `30`.
#' @param null_color Fill colour for the histogram bars, representing the
#'   null distribution -- same role and default (`"grey40"`) as
#'   [plot_robustness()]'s `null_color=`.
#'
#' @return A `ggplot` object (add further layers/themes with `+` as usual).
#'
#' @seealso [compare_knockout_to_null()], [knockout_robustness_null_removal()],
#'   [knockout_robustness()], [plot_r_index_null()]
#' @export
plot_knockout_null <- function(null_df, observed = NULL, index = NULL, bins = 30,
                                null_color = "grey40") {
  idx_cols <- setdiff(names(null_df), c("group_id", "group", "n_removed"))
  if (is.null(index)) index <- idx_cols
  unknown <- setdiff(index, idx_cols)
  if (length(unknown) > 0) {
    stop("index has name(s) not found in null_df: ", paste(unknown, collapse = ", "),
         ". Available columns: ", paste(idx_cols, collapse = ", "), ".", call. = FALSE)
  }

  long <- do.call(rbind, lapply(index, function(ix) {
    data.frame(n_removed = null_df$n_removed, Index = ix, .value = null_df[[ix]])
  }))

  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$.value)) +
    ggplot2::geom_histogram(bins = bins, fill = null_color, colour = "white")

  size_labeller <- function(x) paste("Group size:", x)
  if (length(index) > 1) {
    p <- p + ggplot2::facet_grid(
      Index ~ n_removed, scales = "free",
      labeller = ggplot2::labeller(n_removed = size_labeller)
    )
  } else {
    p <- p + ggplot2::facet_wrap(~n_removed, scales = "free", labeller = ggplot2::as_labeller(size_labeller))
  }

  if (!is.null(observed)) {
    observed <- .drop_intact_row(observed)
    missing_sizes <- setdiff(unique(observed$n_removed), unique(null_df$n_removed))
    if (length(missing_sizes) > 0) {
      warning(length(missing_sizes), " observed group size(s) have no matching ",
              "null replicates in null_df, so those line(s) will show with no ",
              "histogram behind them: ", paste(missing_sizes, collapse = ", "),
              ".", call. = FALSE)
    }
    obs_label <- vapply(observed$group, paste, character(1), collapse = ", ")
    obs_long <- do.call(rbind, lapply(index, function(ix) {
      data.frame(n_removed = observed$n_removed, Index = ix, .value = observed[[ix]], .label = obs_label)
    }))
    p <- p + ggplot2::geom_vline(
      data = obs_long,
      ggplot2::aes(xintercept = .data$.value, colour = .data$.label),
      linewidth = 1, linetype = "dashed"
    )
  }

  p +
    ggplot2::labs(
      x = if (length(index) == 1) paste0(index, " across null replicates") else "Value across null replicates",
      y = "Count",
      colour = "Observed group",
      caption = if (!is.null(observed)) "Dashed lines: observed group(s), faceted by group size" else NULL
    ) +
    ggplot2::theme_bw()
}
