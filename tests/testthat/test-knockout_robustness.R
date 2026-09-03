g <- make_graph("Zachary")
V(g)$name <- paste0("node_", seq_len(vcount(g)))

test_that("groups = NULL tests every vertex individually", {
  res <- knockout_robustness(g, keep = c("giant_component_size"), include_intact = FALSE)
  expect_equal(nrow(res), vcount(g))
  expect_true(all(res$n_removed == 1))
  expect_true(is.list(res$group))
})

test_that("explicit groups can mix name-based and index-based specification", {
  res <- knockout_robustness(g, groups = list(c("node_1", "node_2"), c(3), c(4, 5, 6)),
                              keep = c("giant_component_size"), include_intact = FALSE)
  expect_equal(nrow(res), 3)
  expect_equal(res$group[[1]], c("node_1", "node_2"))
  expect_equal(res$group[[2]], "node_3")
  expect_equal(res$group[[3]], c("node_4", "node_5", "node_6"))
  expect_equal(res$n_removed, c(2, 1, 3))
})

test_that("a bare vector is shorthand for a single group", {
  res <- knockout_robustness(g, groups = c(1, 2), keep = c("giant_component_size"), include_intact = FALSE)
  expect_equal(nrow(res), 1)
  expect_equal(res$group[[1]], c("node_1", "node_2"))
})

test_that("duplicate names/indices within a group are deduplicated with a warning", {
  expect_warning(
    res <- knockout_robustness(g, groups = list(c(1, 1, 2), c(3)), keep = c("giant_component_size"),
                                include_intact = FALSE),
    "deduplicated"
  )
  expect_equal(res$group[[1]], c("node_1", "node_2"))
  expect_equal(res$n_removed[1], 2)
})

test_that("out-of-range index errors", {
  expect_error(knockout_robustness(g, groups = list(c(1, 100))), "out-of-range")
})

test_that("unrecognised vertex name errors", {
  expect_error(knockout_robustness(g, groups = list("not_a_node")), "not found in g")
})

test_that("an empty group errors", {
  expect_error(knockout_robustness(g, groups = list(character(0))), "empty")
})

test_that("each trial is independent -- never accumulates across groups", {
  res <- knockout_robustness(g, groups = list(c(1), c(2)), keep = c("giant_component_size"),
                              include_intact = FALSE)
  g_minus1 <- delete_vertices(g, "node_1")
  g_minus2 <- delete_vertices(g, "node_2")
  expect_equal(res$giant_component_size[1], max(components(g_minus1)$csize))
  expect_equal(res$giant_component_size[2], max(components(g_minus2)$csize))
})

test_that("robustness()/edge_robustness() are unaffected by the .compute_indicator_row() extraction", {
  set.seed(1)
  d <- attack_by_degree(g)
  r1 <- robustness(g, ids = d, keep = c("giant_component_size"))
  expect_equal(r1$giant_component_size[1], vcount(g))

  ord <- random_edge_failures(g)
  r2 <- edge_robustness(g, ids = ord, keep = c("giant_component_size"))
  expect_equal(r2$giant_component_size[1], vcount(g))
})

test_that("include_intact = TRUE (the default) prepends an intact-network reference row", {
  res <- knockout_robustness(g, groups = list(c(1), c(2)), keep = c("giant_component_size"))
  expect_equal(nrow(res), 3)
  expect_equal(res$group_id[1], 0L)
  expect_equal(res$n_removed[1], 0L)
  expect_equal(res$group[[1]], character(0))
  expect_equal(res$giant_component_size[1], vcount(g))
  # the rest still line up exactly as with include_intact = FALSE, just shifted by one row
  expect_equal(res$group_id[2:3], c(1L, 2L))

  res_no_baseline <- knockout_robustness(g, groups = list(c(1), c(2)), keep = c("giant_component_size"),
                                          include_intact = FALSE)
  expect_equal(res$giant_component_size[2:3], res_no_baseline$giant_component_size)
})

test_that("the intact reference row is never picked up as the most damaging group", {
  res <- knockout_robustness(g, keep = c("giant_component_size"))
  worst <- res[which.min(res$giant_component_size), ]
  expect_false(worst$group_id == 0)
})

test_that("knockout_robustness_null_removal() builds a size-stratified null", {
  null_dist <- knockout_robustness_null_removal(g, sizes = c(1, 2), n_reps = 10, seed = 1,
                                                 keep = c("giant_component_size"))
  expect_equal(nrow(null_dist), 20)
  expect_equal(as.integer(table(null_dist$n_removed)), c(10, 10))
  expect_true(is.list(null_dist$group))
  expect_false(0 %in% null_dist$n_removed)  # no stray intact-reference row mixed in
})

test_that("knockout_robustness_null_removal() rejects invalid sizes", {
  expect_error(knockout_robustness_null_removal(g, sizes = c(0), n_reps = 5), "positive integers")
  expect_error(knockout_robustness_null_removal(g, sizes = c(100), n_reps = 5), "positive integers")
})

test_that("compare_knockout_to_null() matches observed groups to same-size null replicates", {
  null_dist <- knockout_robustness_null_removal(g, sizes = c(1, 2), n_reps = 20, seed = 1,
                                                 keep = c("giant_component_size"))
  observed <- knockout_robustness(g, groups = list(c(1), c(33, 34)), keep = c("giant_component_size"),
                                   include_intact = FALSE)
  cmp <- compare_knockout_to_null(observed, null_dist, index = "giant_component_size")
  expect_equal(nrow(cmp), 2)
  expect_true(all(c("group_id", "group", "n_removed", "observed", "null_mean", "p_value") %in% names(cmp)))
})

test_that("compare_knockout_to_null() drops an intact reference row with a warning instead of erroring", {
  null_dist <- knockout_robustness_null_removal(g, sizes = c(1, 2), n_reps = 20, seed = 1,
                                                 keep = c("giant_component_size"))
  observed <- knockout_robustness(g, groups = list(c(1), c(33, 34)), keep = c("giant_component_size"))
  expect_warning(
    cmp <- compare_knockout_to_null(observed, null_dist, index = "giant_component_size"),
    "intact-network reference"
  )
  expect_equal(nrow(cmp), 2)
})

test_that("compare_knockout_to_null() errors on an observed size with no matching null", {
  null_dist <- knockout_robustness_null_removal(g, sizes = c(1), n_reps = 10, seed = 1,
                                                 keep = c("giant_component_size"))
  observed <- knockout_robustness(g, groups = list(c(1, 2, 3)), keep = c("giant_component_size"),
                                   include_intact = FALSE)
  expect_error(compare_knockout_to_null(observed, null_dist), "no matching null replicates")
})

test_that("plot_knockout_null() facets by size and accepts observed groups", {
  null_dist <- knockout_robustness_null_removal(g, sizes = c(1, 2), n_reps = 10, seed = 1,
                                                 keep = c("giant_component_size"))
  observed <- knockout_robustness(g, groups = list(c(1), c(33, 34)), keep = c("giant_component_size"),
                                   include_intact = FALSE)
  fig <- plot_knockout_null(null_dist, observed = observed, index = "giant_component_size")
  expect_s3_class(fig, "ggplot")
})

test_that("plot_knockout_null() drops an intact reference row with a warning", {
  null_dist <- knockout_robustness_null_removal(g, sizes = c(1, 2), n_reps = 10, seed = 1,
                                                 keep = c("giant_component_size"))
  observed <- knockout_robustness(g, groups = list(c(1), c(33, 34)), keep = c("giant_component_size"))
  expect_warning(
    fig <- plot_knockout_null(null_dist, observed = observed, index = "giant_component_size"),
    "intact-network reference"
  )
  expect_s3_class(fig, "ggplot")
})

test_that("plot_knockout_null() warns (not errors) on an unmatched observed size", {
  null_dist <- knockout_robustness_null_removal(g, sizes = c(1), n_reps = 10, seed = 1,
                                                 keep = c("giant_component_size"))
  observed <- knockout_robustness(g, groups = list(c(1, 2, 3)), keep = c("giant_component_size"),
                                   include_intact = FALSE)
  expect_warning(fig <- plot_knockout_null(null_dist, observed = observed), "no matching null replicates")
  expect_s3_class(fig, "ggplot")
})
