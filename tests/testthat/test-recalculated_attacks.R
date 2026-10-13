# Independent reference: remove the current static top vertex by name,
# repeat; once no edges are left, append the rest in their current order.
manual_recalc <- function(g, f) {
  out <- character(0)
  while (vcount(g) > 0) {
    if (ecount(g) == 0) {
      out <- c(out, V(g)$name)
      break
    }
    top <- names(f(g))[1]
    out <- c(out, top)
    g <- delete_vertices(g, top)
  }
  out
}

# Hand-built graph where recalculation changes the order: statically P and Q
# tie on degree 3 (P first by index), but P loses its link to the hub H, so
# once H is gone Q (3) outranks P (2).
g_pq <- make_graph(~ H - P, H - f, H - g, H - h, P - a, P - b, Q - c, Q - d, Q - e)

set.seed(7)
g_dir <- sample_gnm(20, 50, directed = TRUE)
V(g_dir)$name <- paste0("v", seq_len(vcount(g_dir)))
E(g_dir)$weight <- runif(ecount(g_dir), 0.1, 2)

test_that("static and recalculated orders coincide where they must", {
  star <- make_star(6, mode = "undirected")
  expect_identical(attack_by_degree(star, recalculate = TRUE), attack_by_degree(star))
  k5 <- make_full_graph(5)
  expect_identical(attack_by_degree(k5, recalculate = TRUE), attack_by_degree(k5))
  expect_identical(attack_by_betweenness(star, recalculate = TRUE), attack_by_betweenness(star))
})

test_that("recalculation changes the order on a hand-built graph", {
  expect_equal(names(attack_by_degree(g_pq))[1:3], c("H", "P", "Q"))
  expect_equal(names(attack_by_degree(g_pq, recalculate = TRUE)),
               c("H", "Q", "P", "f", "g", "h", "a", "b", "c", "d", "e"))
})

test_that("output has the static format: original indices named by vertex", {
  data(karate, package = "netRobust", envir = environment())
  r <- attack_by_degree(karate, recalculate = TRUE)
  expect_type(r, "integer")
  expect_setequal(unname(r), seq_len(vcount(karate)))
  expect_identical(names(r), V(karate)$name[r])
  expect_equal(nrow(robustness(karate, r, keep = "giant_component_size")), vcount(karate) + 1)
  # unnamed graphs get the same node_<i> fallback as removal_order()
  r0 <- attack_by_degree(make_ring(4), recalculate = TRUE)
  expect_identical(names(r0), paste0("node_", r0))
})

test_that("mode, directed, weights and weights_as are applied at every step", {
  expect_equal(names(attack_by_degree(g_dir, mode = "in", recalculate = TRUE)),
               manual_recalc(g_dir, function(x) attack_by_degree(x, mode = "in")))
  expect_equal(names(attack_by_closeness(g_dir, mode = "out", recalculate = TRUE)),
               manual_recalc(g_dir, function(x) attack_by_closeness(x, mode = "out")))
  expect_equal(names(attack_by_betweenness(g_dir, weights_as = "strength", recalculate = TRUE)),
               manual_recalc(g_dir, function(x) attack_by_betweenness(x, weights_as = "strength")))
  expect_equal(names(attack_by_pagerank(g_dir, directed = FALSE, recalculate = TRUE)),
               manual_recalc(g_dir, function(x) attack_by_pagerank(x, directed = FALSE)))
})

test_that("an explicit weights vector shrinks with the network", {
  w <- E(g_dir)$weight
  g_nw <- delete_edge_attr(g_dir, "weight")
  expect_identical(attack_by_betweenness(g_nw, weights = w, recalculate = TRUE),
                   attack_by_betweenness(g_dir, recalculate = TRUE))
  expect_error(attack_by_betweenness(g_nw, weights = w[-1], recalculate = TRUE),
               "one value per edge")
})

test_that("ties are deterministic for degree-type measures", {
  ring <- make_ring(8)  # every vertex ties at every step
  expect_identical(attack_by_degree(ring, recalculate = TRUE),
                   attack_by_degree(ring, recalculate = TRUE))
  # node_1 goes first (all tie); its neighbours drop to degree 1, so the
  # lowest-index vertex still at degree 2 (node_3) is next
  expect_identical(names(attack_by_degree(ring, recalculate = TRUE))[1:2],
                   c("node_1", "node_3"))
})

test_that("recalculated HITS is reproducible with set.seed()", {
  # Recalculation reaches subgraphs such as B -> D -> E whose leading HITS
  # eigenvalue is repeated; there ARPACK's random start decides the scores,
  # so only a fixed seed (not the tie-break) makes the order reproducible.
  fw <- make_graph(~ A -+ C, B -+ C, C -+ D, B -+ D, D -+ E)
  set.seed(3); o1 <- attack_by_hits(fw, "authority", recalculate = TRUE)
  set.seed(3); o2 <- attack_by_hits(fw, "authority", recalculate = TRUE)
  expect_identical(o1, o2)
  set.seed(3); o3 <- attack_by_hits(fw, "hub", weights = NA, recalculate = TRUE)
  set.seed(3); o4 <- attack_by_hits(fw, "hub", recalculate = TRUE)
  expect_identical(o3, o4)
})

test_that("graphs without edges fall back to index order", {
  e4 <- make_empty_graph(4, directed = FALSE)
  expect_identical(unname(attack_by_eigenvector(e4, recalculate = TRUE)), 1:4)
  expect_identical(unname(attack_by_closeness(e4, recalculate = TRUE)), 1:4)
})

test_that("warnings raised at every step are issued once", {
  w <- testthat::capture_warnings(attack_by_degree(make_ring(5), directed = TRUE,
                                                   recalculate = TRUE))
  expect_length(w, 1)
  expect_match(w, "undirected graph")
})

test_that("recalculated attacks work inside robustness_null_structural()", {
  data(karate, package = "netRobust", envir = environment())
  nul <- robustness_null_structural(karate,
                                    attack_fn = function(g) attack_by_degree(g, recalculate = TRUE),
                                    n_reps = 3, parallel = FALSE, seed = 1,
                                    keep = "giant_component_size")
  expect_equal(sort(unique(nul$rep)), 1:3)
  expect_equal(nrow(nul), 3 * (vcount(karate) + 1))
})

test_that("recalculate must be a single TRUE/FALSE", {
  expect_error(attack_by_degree(g_pq, recalculate = NA), "TRUE or FALSE")
  expect_error(attack_by_pagerank(g_pq, recalculate = "yes"), "TRUE or FALSE")
})

manual_edge_recalc <- function(g) {
  g <- netRobust:::.prepare_edge_graph(g)
  out <- character(0)
  while (ecount(g) > 0) {
    top <- names(attack_by_edge_betweenness(g))[1]
    out <- c(out, top)
    g <- delete_edges(g, top)
  }
  out
}

test_that("recalculated edge betweenness matches a step-by-step reference", {
  data(karate, package = "netRobust", envir = environment())
  r <- attack_by_edge_betweenness(karate, recalculate = TRUE)
  expect_equal(names(r), manual_edge_recalc(karate))
  expect_setequal(unname(r), seq_len(ecount(karate)))
  expect_false(identical(r, attack_by_edge_betweenness(karate)))
  expect_equal(nrow(edge_robustness(karate, r, keep = "giant_component_size")),
               nrow(edge_robustness(karate, attack_by_edge_betweenness(karate),
                                    keep = "giant_component_size")))
})

test_that("recalculated edge betweenness honours an explicit weights vector", {
  w <- E(g_dir)$weight
  g_nw <- delete_edge_attr(g_dir, "weight")
  expect_identical(attack_by_edge_betweenness(g_nw, weights = w, recalculate = TRUE),
                   attack_by_edge_betweenness(g_dir, recalculate = TRUE))
})
