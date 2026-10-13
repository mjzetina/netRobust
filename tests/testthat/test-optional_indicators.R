# Weighted, directed toy network with weights well below 1 (like diet
# proportions), so a silently weighted radius is easy to tell from hops.
set.seed(42)
gw <- sample_gnm(25, 80, directed = TRUE)
V(gw)$name <- paste0("n", seq_len(vcount(gw)))
E(gw)$weight <- runif(ecount(gw), 0.01, 0.5)
ids_w <- random_failures(gw)

hop_radius <- function(g) igraph::radius(g, mode = "all", weights = NA)

test_that("optional radius is the hop-count radius on a weighted graph by default", {
  res <- robustness(gw, ids_w, directed = TRUE, mode = "all", optional = "radius")
  expect_equal(res$radius[1], hop_radius(gw))
  # the buggy behaviour (weights = NULL -> E(g)$weight) gives a different value here
  expect_false(isTRUE(all.equal(hop_radius(gw), igraph::radius(gw, mode = "all"))))
})

test_that("optional radius follows weights_as, like diameter/mean_distance", {
  g_dist <- robustness(gw, ids_w, directed = TRUE, mode = "all",
                       weights_as = "distance", optional = "radius")
  expect_equal(g_dist$radius[1], igraph::radius(gw, mode = "all", weights = E(gw)$weight))

  g_str <- robustness(gw, ids_w, directed = TRUE, mode = "all",
                      weights_as = "strength", optional = "radius")
  expect_equal(g_str$radius[1], igraph::radius(gw, mode = "all", weights = 1 / E(gw)$weight))
})

test_that("optional radius honours an explicit weights= vector", {
  w <- runif(ecount(gw), 1, 3)
  res <- robustness(gw, ids_w, directed = TRUE, mode = "all", weights = w, optional = "radius")
  expect_equal(res$radius[1], igraph::radius(gw, mode = "all", weights = w))
})

test_that("optional_robustness_indicators() radius is topological unless weight_attr is given", {
  rad <- optional_robustness_indicators(names = "radius")$radius
  expect_equal(rad(gw), hop_radius(gw))

  gw2 <- igraph::set_edge_attr(gw, "w2", value = 1 / E(gw)$weight)
  rad_w <- optional_robustness_indicators(names = "radius", weight_attr = "w2")$radius
  expect_equal(rad_w(gw2), igraph::radius(gw2, mode = "all", weights = E(gw2)$w2))
})

test_that("edge_robustness() radius also ignores the weight attribute by default", {
  ord <- random_edge_failures(gw)
  res <- edge_robustness(gw, ids = ord, directed = TRUE, mode = "all", optional = "radius")
  expect_equal(res$radius[1], hop_radius(gw))
})
