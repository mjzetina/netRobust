test_that(".use_parallel() requires a real, non-sequential future plan with >1 worker", {
  skip_if_not_installed("future.apply")
  skip_if_not_installed("future")

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)

  future::plan(future::sequential)
  expect_false(netRobust:::.use_parallel(TRUE))

  future::plan(future::multisession, workers = 2)
  expect_true(netRobust:::.use_parallel(TRUE))

  expect_false(netRobust:::.use_parallel(FALSE))
})

test_that("null-model functions run sequentially (with no error) when no plan is set", {
  g <- make_graph("Zachary")
  res <- robustness_null_removal(g, n_reps = 3, seed = 1, keep = c("giant_component_size"))
  expect_equal(length(unique(res$rep)), 3)
})

test_that("summarise_null() attaches the quantile levels actually used", {
  g <- make_graph("Zachary")
  null_dist <- robustness_null_removal(g, n_reps = 10, seed = 1, keep = c("giant_component_size"))

  ne_default <- summarise_null(null_dist)
  expect_equal(attr(ne_default, "probs"), c(lower = 0.025, median = 0.5, upper = 0.975))

  custom_probs <- c(lower = 0.1, median = 0.5, upper = 0.9)
  ne_custom <- summarise_null(null_dist, probs = custom_probs)
  expect_equal(attr(ne_custom, "probs"), custom_probs)
})
