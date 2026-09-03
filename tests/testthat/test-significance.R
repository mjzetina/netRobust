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

# --- index = NULL / multi-index support across the R-index family ----------
g <- make_graph("Zachary")
d <- attack_by_degree(g)
attack <- robustness(g, ids = d, keep = c("giant_component_size", "mean_distance", "connectivity"))
null_dist <- robustness_null_removal(g, n_reps = 10, seed = 1,
                                      keep = c("giant_component_size", "mean_distance", "connectivity"))

test_that("robustness_index(): single index stays a bare scalar, multi-index is a named vector", {
  single <- robustness_index(attack, index = "giant_component_size")
  expect_true(is.numeric(single) && length(single) == 1 && is.null(names(single)))

  all3 <- robustness_index(attack)
  expect_true(is.numeric(all3) && length(all3) == 3)
  expect_equal(names(all3), c("giant_component_size", "mean_distance", "connectivity"))

  two <- robustness_index(attack, index = c("giant_component_size", "mean_distance"))
  expect_equal(names(two), c("giant_component_size", "mean_distance"))
  expect_equal(unname(two["giant_component_size"]), single)
})

test_that("robustness_index_null(): same scalar/vector rule, matches per-index single calls", {
  single <- robustness_index_null(null_dist, index = "giant_component_size")
  expect_true(length(single) == 1 && is.null(names(single)))

  all3 <- robustness_index_null(null_dist)
  expect_equal(names(all3), c("giant_component_size", "mean_distance", "connectivity"))
  expect_equal(unname(all3["giant_component_size"]), single)
})

test_that("compare_robustness_index_to_null(): always returns an Index column, one row per index", {
  single <- compare_robustness_index_to_null(attack, null_dist, index = "giant_component_size")
  expect_equal(nrow(single), 1)
  expect_equal(single$Index, "giant_component_size")

  all3 <- compare_robustness_index_to_null(attack, null_dist)
  expect_equal(nrow(all3), 3)
  expect_setequal(all3$Index, c("giant_component_size", "mean_distance", "connectivity"))
  expect_equal(all3$observed[all3$Index == "giant_component_size"], single$observed)
})

test_that("compare_robustness(): index = NULL uses the intersection of columns across scenarios", {
  b <- attack_by_betweenness(g)
  attack_b <- robustness(g, ids = b, keep = c("giant_component_size", "mean_distance"))

  cmp <- compare_robustness(list(Degree = attack, Betweenness = attack_b))
  expect_setequal(unique(cmp$Index), c("giant_component_size", "mean_distance"))
  expect_equal(nrow(cmp), 4)  # 2 scenarios x 2 shared indicators

  cmp_one <- compare_robustness(list(Degree = attack, Betweenness = attack_b),
                                 index = "giant_component_size")
  expect_equal(nrow(cmp_one), 2)
  expect_true(all(cmp_one$Index == "giant_component_size"))
})

test_that("compare_robustness() errors clearly when scenarios share no indicator columns", {
  attack_only_gc <- robustness(g, ids = d, keep = c("giant_component_size"))
  attack_only_md <- robustness(g, ids = d, keep = c("mean_distance"))
  expect_error(
    compare_robustness(list(A = attack_only_gc, B = attack_only_md)),
    "share no common indicator columns"
  )
})

test_that("plot_r_index_null(): facets by Index when more than one is requested", {
  fig_single <- plot_r_index_null(null_dist, observed = attack, index = "giant_component_size")
  expect_s3_class(fig_single, "ggplot")

  fig_multi <- plot_r_index_null(null_dist, observed = attack)
  expect_s3_class(fig_multi, "ggplot")
  expect_setequal(unique(fig_multi$data$Index), c("giant_component_size", "mean_distance", "connectivity"))
})
