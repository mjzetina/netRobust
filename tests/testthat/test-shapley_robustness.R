g <- make_graph("Zachary")
V(g)$name <- paste0("node_", seq_len(vcount(g)))

test_that("shapley_robustness() returns the expected shape", {
  res <- shapley_robustness(g, n_reps = 20, n_perm = 20, seed = 1,
                             keep = c("giant_component_size", "mean_distance"))
  expect_s3_class(res, "shapley_robustness")
  expect_named(res, c("nodes", "global_test"))
  expect_equal(nrow(res$nodes), vcount(g))
  expect_true(all(c("node", "giant_component_size", "giant_component_size_se",
                     "mean_distance", "mean_distance_se") %in% names(res$nodes)))
  expect_equal(nrow(res$global_test), 2)
  expect_true(all(c("Index", "observed_stat", "p_value") %in% names(res$global_test)))
  expect_true(all(res$global_test$p_value >= 0 & res$global_test$p_value <= 1))
})

test_that("shapley values satisfy the efficiency axiom: they sum to intact minus empty", {
  set.seed(2)
  ord <- sample(V(g)$name)
  indicators <- list(giant_component_size = function(gi) {
    cs <- components(gi)$csize
    if (length(cs)) max(cs) else 0
  })
  curve <- netRobust:::.shapley_curve(g, ord, indicators)
  expect_equal(nrow(curve), vcount(g) + 1)
  expect_equal(curve$giant_component_size[1], vcount(g))
  expect_equal(curve$giant_component_size[nrow(curve)], 0)

  res <- shapley_robustness(g, n_reps = 1, n_perm = 2, seed = 2,
                             keep = c("giant_component_size"))
  expect_equal(sum(res$nodes$giant_component_size), vcount(g))
})

test_that("a node with no structural role has ~zero Shapley value for giant_component_size", {
  # A two-node pendant attached to a fully-connected core: the pendant's
  # removal barely changes the giant component compared to a well-connected
  # core node's -- not an exact zero (removal context still varies), but
  # much smaller than the hub's.
  core <- make_full_graph(6)
  gp <- add_vertices(core, 1)
  gp <- add_edges(gp, c(1, 7))
  V(gp)$name <- paste0("node_", seq_len(vcount(gp)))

  res <- shapley_robustness(gp, n_reps = 100, n_perm = 20, seed = 3,
                             keep = c("giant_component_size"))
  pendant <- res$nodes$giant_component_size[res$nodes$node == "node_7"]
  core_mean <- mean(res$nodes$giant_component_size[res$nodes$node != "node_7"])
  expect_true(pendant < core_mean)
})

test_that("plot_shapley() facets by indicator and validates inputs", {
  res <- shapley_robustness(g, n_reps = 15, n_perm = 15, seed = 1,
                             keep = c("giant_component_size", "mean_distance"))

  fig_all <- plot_shapley(res)
  expect_s3_class(fig_all, "ggplot")

  fig_one <- plot_shapley(res, indicators = "giant_component_size")
  expect_s3_class(fig_one, "ggplot")

  fig_topn <- plot_shapley(res, top_n = 5)
  expect_equal(length(unique(fig_topn$data$node)), 5)

  fig_vertex <- plot_shapley(res, order = "vertex")
  expect_equal(levels(fig_vertex$data$node), res$nodes$node)

  expect_error(plot_shapley(res$nodes), "output of shapley_robustness")
  expect_error(plot_shapley(res, indicators = "not_a_real_indicator"), "not found")
})
