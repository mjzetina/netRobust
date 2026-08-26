g <- make_graph("Zachary")
# Match the same "node_1", "node_2", ... fallback .prepare_edge_graph() (and
# robustness()) assign internally when V(g)$name is NULL, so ends()-based
# comparisons below use the same identifiers the package actually produces.
V(g)$name <- paste0("node_", seq_len(vcount(g)))

test_that("random_edge_failures() returns a full, edge-named removal order", {
  set.seed(1)
  ord <- random_edge_failures(g)
  expect_length(ord, ecount(g))
  expect_true(all(grepl("\\|", names(ord))))
})

test_that("edge_robustness() removes edges only -- starts at the intact GC", {
  set.seed(1)
  ord <- random_edge_failures(g)
  res <- edge_robustness(g, ids = ord, keep = c("GC", "Density"))
  expect_equal(nrow(res), ecount(g))
  expect_equal(res$GC[1], vcount(g))  # no edges removed yet: still one giant component
  expect_true(all(diff(res$Density) <= 1e-9))  # non-increasing as edges are removed
})

test_that("edge_robustness() output has the same shape as robustness()'s", {
  set.seed(1)
  d <- attack_by_degree(g)
  node_res <- robustness(g, ids = d, keep = c("GC", "APL", "Connectivity"))
  ord <- random_edge_failures(g)
  edge_res <- edge_robustness(g, ids = ord, keep = c("GC", "APL", "Connectivity"))
  expect_equal(names(node_res), names(edge_res))
})

test_that("attack_by_edge_betweenness() removes the highest-betweenness edge first", {
  ord <- attack_by_edge_betweenness(g)
  eb <- edge_betweenness(g)
  top_pair <- ends(g, E(g)[which.max(eb)])
  expect_true(paste(top_pair[1, 1], top_pair[1, 2], sep = "|") == names(ord)[1])
})

test_that("attack_by_edge_weight() requires a weighted graph", {
  expect_error(attack_by_edge_weight(g), "requires a weighted graph")
})

test_that("attack_by_edge_weight() removes the heaviest edge first by default", {
  gw <- g
  set.seed(1)
  E(gw)$weight <- runif(ecount(gw))
  ord <- attack_by_edge_weight(gw)
  heaviest_pair <- ends(gw, E(gw)[which.max(E(gw)$weight)])
  expect_true(paste(heaviest_pair[1, 1], heaviest_pair[1, 2], sep = "|") == names(ord)[1])
})

test_that("attack_by_edge_weight(decreasing = FALSE) removes the weakest edge first", {
  gw <- g
  set.seed(1)
  E(gw)$weight <- runif(ecount(gw))
  ord <- attack_by_edge_weight(gw, decreasing = FALSE)
  lightest_pair <- ends(gw, E(gw)[which.min(E(gw)$weight)])
  expect_true(paste(lightest_pair[1, 1], lightest_pair[1, 2], sep = "|") == names(ord)[1])
})

test_that("attack_by_edge_weight(weights_as = \"distance\") inverts before ranking", {
  gw <- g
  set.seed(1)
  E(gw)$weight <- runif(ecount(gw))
  # Under "distance" semantics, the LOWEST raw weight is the strongest tie,
  # so it should be removed first when decreasing = TRUE (the default).
  ord <- attack_by_edge_weight(gw, weights_as = "distance")
  lightest_pair <- ends(gw, E(gw)[which.min(E(gw)$weight)])
  expect_true(paste(lightest_pair[1, 1], lightest_pair[1, 2], sep = "|") == names(ord)[1])
})

test_that("parallel edges are collapsed by default, with a warning, summing weights", {
  el <- rbind(c("A", "B"), c("A", "B"), c("B", "C"))
  gp <- graph_from_edgelist(el, directed = TRUE)
  E(gp)$weight <- c(2, 5, 1)

  expect_warning(ord <- random_edge_failures(gp), "redundant edge")
  expect_length(ord, 2)  # 3 raw edges collapse to 2 (A|B merged)
})

test_that("on_parallel = \"error\" stops instead of collapsing", {
  el <- rbind(c("A", "B"), c("A", "B"), c("B", "C"))
  gp <- graph_from_edgelist(el, directed = TRUE)
  expect_error(random_edge_failures(gp, on_parallel = "error"), "redundant")
})

test_that("robustness() is unaffected by the shared-loop refactor (regression check)", {
  set.seed(1)
  d <- attack_by_degree(g)
  res <- robustness(g, ids = d, keep = c("GC"))
  expect_equal(nrow(res), vcount(g))
  expect_equal(res$GC[1], vcount(g))
  expect_true(all(diff(res$GC) <= 0))  # GC is non-increasing under node removal
})
