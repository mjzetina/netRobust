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
