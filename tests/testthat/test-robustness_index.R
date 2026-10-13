# Schneider et al. (2011), Eq. 1: for a connected graph attacked by degree,
# R = (1/N) sum_{Q=1}^{N} s(Q), with analytic values (N-1)/N^2 for a star
# and (N-1)/(2N) for a complete graph.
r_gc <- function(g) {
  robustness_index(robustness(g, attack_by_degree(g), keep = "giant_component_size"))
}

test_that("R-index matches Schneider et al.'s analytic values for a star", {
  for (n in c(10, 50)) {
    expect_equal(r_gc(make_star(n, mode = "undirected")), (n - 1) / n^2, tolerance = 1e-12)
  }
})

test_that("R-index matches Schneider et al.'s analytic values for a complete graph", {
  for (n in c(10, 50)) {
    expect_equal(r_gc(make_full_graph(n)), (n - 1) / (2 * n), tolerance = 1e-12)
  }
})

test_that("R-index averages Q = 1..N, skipping undefined steps", {
  data(karate, package = "netRobust", envir = environment())
  res <- robustness(karate, attack_by_degree(karate),
                    keep = c("giant_component_size", "mean_distance"))
  gc <- res$giant_component_size
  expect_equal(robustness_index(res, "giant_component_size"),
               sum(gc[-1] / gc[1]) / vcount(karate))
  md <- res$mean_distance
  expect_true(is.nan(md[nrow(res)]))  # undefined on the empty network
  expect_equal(robustness_index(res, "mean_distance"), mean(md[-1] / md[1], na.rm = TRUE))
})

test_that("R-index errors on curves without n_removed (netRobust < 0.3.0)", {
  old <- data.frame(N = 1:3, giant_component_size = c(3, 2, 1))
  expect_error(robustness_index(old), "n_removed")
})
