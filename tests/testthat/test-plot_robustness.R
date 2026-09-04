g <- make_graph("Zachary")
r <- random_failures(g)
d <- attack_by_degree(g)
attack <- robustness(g, ids = d, keep = c("giant_component_size"))
failure_draw <- robustness(g, ids = r, keep = c("giant_component_size"))
null_dist <- robustness_null_removal(g, n_reps = 10, seed = 1, keep = c("giant_component_size"))
null_env <- summarise_null(null_dist)

test_that("failure = NULL derives the curve from null_envelope and labels it with summary=", {
  fig <- plot_robustness(attack = attack, null_envelope = null_env)
  expect_s3_class(fig, "ggplot")
  expect_true("Failures (median)" %in% fig$data$Scenario)

  fig_mean <- plot_robustness(attack = attack, null_envelope = null_env, summary = "mean")
  expect_true("Failures (mean)" %in% fig_mean$data$Scenario)
})

test_that("an explicit failure= is labelled plainly, even alongside null_envelope", {
  fig <- plot_robustness(attack = attack, failure = failure_draw)
  expect_true("Failures" %in% fig$data$Scenario)

  fig2 <- plot_robustness(attack = attack, failure = failure_draw, null_envelope = null_env)
  expect_true("Failures" %in% fig2$data$Scenario)
  expect_false(any(grepl("median|mean", fig2$data$Scenario)))
})

test_that("failure_label= overrides the default in both the derived and explicit cases", {
  fig <- plot_robustness(attack = attack, null_envelope = null_env, failure_label = "Custom")
  expect_true("Custom" %in% fig$data$Scenario)

  fig2 <- plot_robustness(attack = attack, failure = failure_draw, failure_label = "Custom2")
  expect_true("Custom2" %in% fig2$data$Scenario)
})

test_that("failure = NULL and null_envelope = NULL together errors", {
  expect_error(plot_robustness(attack = attack), "failure must be supplied")
})

test_that("the caption names the actual summary used when derived, and stays generic otherwise", {
  fig <- plot_robustness(attack = attack, null_envelope = null_env, summary = "mean")
  expect_true(grepl("mean", fig$labels$caption))

  fig2 <- plot_robustness(attack = attack, failure = failure_draw, null_envelope = null_env)
  expect_false(grepl("median|mean", fig2$labels$caption))
})

test_that("the caption reports the envelope's true coverage, not a hardcoded 95%", {
  fig95 <- plot_robustness(attack = attack, null_envelope = null_env)
  expect_true(grepl("95% envelope", fig95$labels$caption))

  null_env80 <- summarise_null(null_dist, probs = c(lower = 0.1, median = 0.5, upper = 0.9))
  fig80 <- plot_robustness(attack = attack, null_envelope = null_env80)
  expect_true(grepl("80% envelope", fig80$labels$caption))
})
