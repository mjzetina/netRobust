# Network robustness through failures and attack scenarios ####
# Manuel J. Zetina-Rejon
# Plot robustness curves, optionally with a Monte Carlo null envelope
# (pass the output of significance.R::summarise_null() as null_envelope)

#' Plot robustness curves for failures and attack scenarios
#'
#' Draws the failure and attack robustness curves side by side, one panel
#' per indicator, optionally with a shaded Monte Carlo null envelope
#' (see [robustness_null_removal()] / [summarise_null()]) behind them.
#'
#' @param attack Output of [robustness()]/[edge_robustness()] for the attack
#'   scenario.
#' @param failure Output of [robustness()]/[edge_robustness()] for the
#'   failure scenario, OR `NULL` (default) to derive it automatically from
#'   `null_envelope` -- see `summary=` below. Supplying `failure` explicitly
#'   (e.g. a single [random_failures()] draw) is still supported, including
#'   alongside `null_envelope` (drawn as the shaded band behind it); in that
#'   case the curve is labelled with the plain `"Failures"`, since this
#'   function has no way to verify what statistic, if any, a curve you
#'   supplied yourself represents -- see `failure_label=` to set a more
#'   specific one.
#' @param null_envelope Optional output of [summarise_null()]. If supplied,
#'   drawn as a shaded reference band behind the curves, captioned with its
#'   true coverage (`attr(null_envelope, "probs")`, as set by
#'   [summarise_null()]'s `probs=`) -- 95% unless you customised `probs=`
#'   when building it. When `failure`
#'   is *not* also supplied, it's derived from this null distribution's
#'   `summary=` column instead (`tidyr::pivot_wider(null_envelope, id_cols =
#'   N, names_from = Index, values_from = summary)`), and labelled
#'   `"Failures (<summary>)"` -- guaranteed to match what's actually plotted,
#'   since this function performed the derivation itself, rather than
#'   trusting a `failure=` you pivoted by hand to actually be that statistic.
#' @param summary `"median"` (default) or `"mean"`: which column of
#'   `null_envelope` to use as the failure curve when `failure = NULL`.
#'   Ignored if `failure` is supplied explicitly.
#' @param failure_label Optional character string overriding the failure
#'   curve's legend/colour-scale label (both when derived automatically and
#'   when `failure` is supplied explicitly). `NULL` (default) uses
#'   `"Failures (<summary>)"` when `failure` was derived from
#'   `null_envelope`, or plain `"Failures"` when you supplied `failure`
#'   yourself.
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
#' undefined -- e.g. diameter on an isolated node), this warns once with
#' the specific scenario/indicator/step count affected, before ggplot2's
#' own generic "Removed N rows..." warnings fire at print time.
#'
#' @seealso [robustness()], [robustness_null_removal()], [summarise_null()]
#' @export
plot_robustness <- function(attack, failure = NULL, null_envelope = NULL,
                             summary = c("median", "mean"), failure_label = NULL,
                             scenario_colors = c(Attack = "#1F77B4", Failures = "#D62728"),
                             null_color = "grey40") {
  summary <- match.arg(summary)
  failure_derived <- is.null(failure)

  if (is.null(failure)) {
    if (is.null(null_envelope)) {
      stop("failure must be supplied when null_envelope = NULL -- either pass ",
           "a robustness()/edge_robustness() curve directly as failure=, or ",
           "supply null_envelope= (the output of summarise_null()) and ",
           "failure will be derived from it automatically (see summary=).",
           call. = FALSE)
    }
    failure <- tidyr::pivot_wider(null_envelope, id_cols = "N",
                                   names_from = "Index", values_from = dplyr::all_of(summary))
    auto_label <- paste0("Failures (", summary, ")")
  } else {
    auto_label <- "Failures"
  }
  failure_label <- if (!is.null(failure_label)) failure_label else auto_label

  failure_lon <- tidyr::pivot_longer(failure, -N, names_to = "Index", values_to = "Value")
  attack_lon  <- tidyr::pivot_longer(attack,  -N, names_to = "Index", values_to = "Value")

  failure_lon$Scenario <- failure_label
  attack_lon$Scenario  <- "Attack"

  scenarios <- rbind(failure_lon, attack_lon)

  # Warn once, clearly, about points that won't be plotted -- instead of
  # relying on ggplot2's own generic "Removed N rows..." messages (one per
  # layer, computed at print time, with no explanation of why). NAs here
  # are usually expected, not a bug: several indicators (diameter, mean_distance,
  # degree_assortativity, radius, reciprocity...) become mathematically undefined once
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
            "for an indicator to become undefined (e.g. diameter on an isolated node) ",
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
    # summarise_null() attaches the quantile levels it actually used as a
    # "probs" attribute -- read the true coverage from there instead of
    # assuming 95% (its default, but not necessarily what was used), the
    # same reasoning as failure_label= not presuming a statistic it can't
    # verify. Falls back to "95%" if null_envelope wasn't built that way
    # (e.g. constructed by hand).
    envelope_probs <- attr(null_envelope, "probs")
    envelope_pct <- if (!is.null(envelope_probs) && all(c("lower", "upper") %in% names(envelope_probs))) {
      paste0(round(100 * (envelope_probs[["upper"]] - envelope_probs[["lower"]])), "%")
    } else {
      "95%"
    }
  }

  if (!is.null(scenario_colors)) {
    # scenario_colors is keyed "Failures"/"Attack" by convention; remap the
    # "Failures" key to match failure_label so the colour scale still lines
    # up, without forcing callers to know about the "(median)"/"(mean)" suffix.
    names(scenario_colors)[names(scenario_colors) == "Failures"] <- failure_label
    p <- p + ggplot2::scale_colour_manual(values = scenario_colors)
  }

  p +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1) +
    ggplot2::facet_wrap(~ Index, scales = "free_y") +
    ggplot2::labs(x = "Number of removed nodes", y = "Value",
         caption = if (is.null(null_envelope)) {
           NULL
         } else if (failure_derived) {
           paste0("Line: ", summary, " across null-model replicates. Shaded band: ",
                  envelope_pct, " envelope.")
         } else {
           paste0("Shaded band: ", envelope_pct, " envelope across null-model replicates.")
         }) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}
