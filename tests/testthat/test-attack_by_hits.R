# Toy food web, edges prey -> predator: basal A, B; intermediate C, D; top E.
fw <- make_graph(~ A -+ C, B -+ C, C -+ D, B -+ D, D -+ E)

# Weighted toy where weights change the ranking: unweighted, prey P2 (two
# predators) is the top hub; weighted, P1's single strong link wins.
gw <- make_graph(~ P1 -+ X, P2 -+ X, P2 -+ Y, P3 -+ Y)
E(gw)$weight <- c(10, 1, 1, 1)

test_that("hub on g equals authority on the reversed graph, and vice versa", {
  set.seed(1)
  g <- sample_gnm(30, 90, directed = TRUE)
  V(g)$name <- paste0("n", seq_len(vcount(g)))
  gr <- reverse_edges(g)
  set.seed(2)
  expect_identical(attack_by_hits(g, "hub"), attack_by_hits(gr, "authority"))
  expect_identical(attack_by_hits(g, "authority"), attack_by_hits(gr, "hub"))
  expect_identical(attack_by_hits(g, "product"), attack_by_hits(gr, "product"))
})

test_that("orientation convention on a prey -> predator food web", {
  set.seed(1)
  h <- hits_scores(fw, weights = NA)
  expect_equal(names(attack_by_hits(fw, "hub"))[1], names(which.max(h$hub)))
  expect_equal(names(attack_by_hits(fw, "authority"))[1], names(which.max(h$authority)))
  # basal species never have authority > 0 with prey -> predator edges
  expect_true(all(h$authority[c("A", "B")] == 0))
})

test_that("weighted HITS uses weights as strength and differs from unweighted", {
  set.seed(1)
  w_ord  <- attack_by_hits(gw, "hub")
  uw_ord <- attack_by_hits(gw, "hub", weights = NA)
  expect_equal(names(w_ord)[1], "P1")
  expect_equal(names(uw_ord)[1], "P2")
  # exact non-zero ties (P1, P3 both hub 0.5) resolve by index, whatever the seed
  expect_equal(names(uw_ord), c("P2", "P1", "P3", "X", "Y"))
  # weights = NA gives the same as a graph without the weight attribute
  expect_identical(uw_ord, attack_by_hits(delete_edge_attr(gw, "weight"), "hub"))
  # weights_as = "distance" inverts; an explicit vector overrides weights_as
  expect_identical(attack_by_hits(gw, "hub", weights_as = "distance"),
                   attack_by_hits(gw, "hub", weights = 1 / E(gw)$weight))
  expect_identical(attack_by_hits(gw, "hub", weights = rep(1, 4), weights_as = "distance"),
                   uw_ord)
})

test_that("product ranks like the geometric mean of hub and authority", {
  set.seed(3)
  g <- sample_gnm(25, 80, directed = TRUE)
  set.seed(4)
  h <- hits_scores(g, weights = NA)
  gm <- sqrt(h$hub * h$authority)
  ord <- attack_by_hits(g, "product")
  pos <- gm > 1e-6 * max(gm)
  # the strictly positive part of the order follows the geometric mean
  expect_equal(unname(ord[seq_len(sum(pos))]), order(gm, decreasing = TRUE)[seq_len(sum(pos))])
})

test_that("ties (zero scores) are broken deterministically, independent of the seed", {
  orders <- lapply(1:5, function(s) {
    set.seed(s)
    attack_by_hits(fw, "authority")
  })
  for (o in orders[-1]) expect_identical(o, orders[[1]])
  # among authority-0 vertices, the higher hub score goes first
  set.seed(1)
  h <- hits_scores(fw, weights = NA)
  zero <- names(h$authority)[h$authority == 0]
  o <- names(orders[[1]])
  o0 <- o[o %in% zero]
  expect_false(is.unsorted(rev(h$hub[o0])))
})

test_that("input checks", {
  expect_error(attack_by_hits(fw, "foo"))
  expect_error(attack_by_hits(gw, weights = c(1, 2)), "one value per edge")
  expect_warning(attack_by_hits(make_ring(5)), "undirected")
})
