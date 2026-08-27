g <- make_graph("Zachary")
V(g)$name <- paste0("node_", seq_len(vcount(g)))

test_that("groups = NULL tests every vertex individually", {
  res <- knockout_robustness(g, keep = c("GC"))
  expect_equal(nrow(res), vcount(g))
  expect_true(all(res$n_removed == 1))
  expect_true(is.list(res$group))
})

test_that("explicit groups can mix name-based and index-based specification", {
  res <- knockout_robustness(g, groups = list(c("node_1", "node_2"), c(3), c(4, 5, 6)),
                              keep = c("GC"))
  expect_equal(nrow(res), 3)
  expect_equal(res$group[[1]], c("node_1", "node_2"))
  expect_equal(res$group[[2]], "node_3")
  expect_equal(res$group[[3]], c("node_4", "node_5", "node_6"))
  expect_equal(res$n_removed, c(2, 1, 3))
})

test_that("a bare vector is shorthand for a single group", {
  res <- knockout_robustness(g, groups = c(1, 2), keep = c("GC"))
  expect_equal(nrow(res), 1)
  expect_equal(res$group[[1]], c("node_1", "node_2"))
})

test_that("duplicate names/indices within a group are deduplicated with a warning", {
  expect_warning(
    res <- knockout_robustness(g, groups = list(c(1, 1, 2), c(3)), keep = c("GC")),
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
  res <- knockout_robustness(g, groups = list(c(1), c(2)), keep = c("GC"))
  g_minus1 <- delete_vertices(g, "node_1")
  g_minus2 <- delete_vertices(g, "node_2")
  expect_equal(res$GC[1], max(components(g_minus1)$csize))
  expect_equal(res$GC[2], max(components(g_minus2)$csize))
})

test_that("robustness()/edge_robustness() are unaffected by the .compute_indicator_row() extraction", {
  set.seed(1)
  d <- attack_by_degree(g)
  r1 <- robustness(g, ids = d, keep = c("GC"))
  expect_equal(r1$GC[1], vcount(g))

  ord <- random_edge_failures(g)
  r2 <- edge_robustness(g, ids = ord, keep = c("GC"))
  expect_equal(r2$GC[1], vcount(g))
})
