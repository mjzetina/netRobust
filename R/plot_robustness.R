# Network robustness through failures and attack scenarios ####
# Manuel J. Zetina-Rejon
# Plot robustness curves, optionally with a Monte Carlo null envelope
# (pass the output of significance.R::summarise_null() as null_envelope)

#' Plot robustness curves for failures and attack scenarios
#'
#' Draws the failure and attack robustness curves side by side, one panel
#' per indicator, optionally with a shaded Monte Carlo null envelope
#' (see [robustness_null()] / [summarise_null()]) behind them.
#'
#' @param failure,attack Output of [robustness()] for the failure and
#'   attack scenarios, respectively.
#' @param null_envelope Optional output of [summarise_null()]. If supplied,
#'   drawn as a shaded 95% reference band behind the curves.
#' @param scenario_colors Named character vector of two hex colours, named
#'   `"Attack"` and `"Failures"`, controlling the curve colours. Defaults
#'   to `c(Attack = "#1F77B4", Failures = "#D62728")`. Override to match a
#'   journal's colour scheme, or pass `NULL` to fall back to ggplot2's
#'   default palette instead.
#' @param null_color Fill colour for the null-envelope shaded band, if
#'   `null_envelope` is supplied. Defaults to `"grey40"`. Has no effect
#'   when `null_envelope = NULL`.
#'
#' @return A `ggplot` object (add further layers/themes with `+` as usual).
#'
#' @details
#' If any indicator has `NA` values at some removal steps (common once the
#' network fragments enough that an indicator becomes mathematically
#' undefined -- e.g. Diameter on an isolated node), this warns once with
#' the specific scenario/indicator/step count affected, before ggplot2's
#' own generic "Removed N rows..." warnings fire at print time.
#'
#' @seealso [robustness()], [robustness_null()], [summarise_null()]
#' @export
plot_robustness <- function(failure, attack, null_envelope = NULL,
                             scenario_colors = c(Attack = "#1F77B4", Failures = "#D62728"),
                             null_color = "grey40") {
  failure_lon <- tidyr::pivot_longer(failure, -N, names_to = "Index", values_to = "Value")
  attack_lon  <- tidyr::pivot_longer(attack,  -N, names_to = "Index", values_to = "Value")

  failure_lon$Scenario <- "Failures"
  attack_lon$Scenario  <- "Attack"

  scenarios <- rbind(failure_lon, attack_lon)

  # Warn once, clearly, about points that won't be plotted -- instead of
  # relying on ggplot2's own generic "Removed N rows..." messages (one per
  # layer, computed at print time, with no explanation of why). NAs here
  # are usually expected, not a bug: several indicators (Diameter, APL,
  # Homophyly, Radius, Reciprocity...) become mathematically undefined once
  # the network fragments enough during removal -- e.g. a single isolated
  # node has no diameter -- so those specific points simply have nothing
  # to draw.
  na_curve <- scenarios[is.na(scenarios$Value), c("Scenario", "Index", "N")]
  if (nrow(na_curve) > 0) {
    by_group <- stats::aggregate(N ~ Scenario + Index, na_curve, length)
    detail <- paste0(by_group$Scenario, "/", by_group$Index, " (", by_group$N, " step(s))",
                      collapse = ", ")
    warning(nrow(na_curve), " point(s) are NA and won't be drawn: ", detail,
            ". This is expected once the network fragments enough during removal ",
            "for an indicator to become undefined (e.g. Diameter on an isolated node) ",
            "-- not necessarily a problem.", call. = FALSE)
  }
  if (!is.null(null_envelope)) {
    na_env <- sum(is.na(null_envelope$lower) | is.na(null_envelope$upper))
    if (na_env > 0) {
      warning(na_env, " point(s) in null_envelope are NA and won't be shaded ",
              "(same expected cause as above).", call. = FALSE)
    }
  }

  p <- ggplot2::ggplot(scenarios, ggplot2::aes(x = N, y = Value, colour = Scenario, group = Scenario))

  if (!is.null(null_envelope)) {
    p <- p + ggplot2::geom_ribbon(
      data = null_envelope,
      ggplot2::aes(x = N, ymin = lower, ymax = upper),
      inherit.aes = FALSE, alpha = 0.15, fill = null_color
    )
  }

  if (!is.null(scenario_colors)) {
    p <- p + ggplot2::scale_colour_manual(values = scenario_colors)
  }

  p +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1) +
    ggplot2::facet_wrap(~ Index, scales = "free_y") +
    ggplot2::labs(x = "Number of removed nodes", y = "Value",
         caption = if (!is.null(null_envelope)) "Shaded band: 95% envelope of random-failure replicates" else NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}
