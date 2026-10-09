# netRobust 0.1.0.9000

## New features

* `attack_by_hits()` removes vertices in decreasing order of Kleinberg's HITS
  hub score, authority score, or their product (`which = "hub"`,
  `"authority"`, `"product"`), via `igraph::hits_scores()`. Weights are used
  as strength by default and follow the usual `weights` / `weights_as`
  conventions; `weights = NA` forces unweighted HITS on a weighted graph.
  Ties (common on near-acyclic food webs, where many scores are exactly zero)
  are broken deterministically. netRobust now requires igraph >= 2.1.0.

## Bug fixes

* The optional `radius` indicator no longer silently uses a graph's `weight`
  edge attribute as edge lengths. `igraph::radius()` with `weights = NULL`
  picks up `E(g)$weight` on weighted graphs, so `radius` was weighted by
  accident while `diameter`, `mean_distance` and `connectivity` were
  topological by default. `radius` now follows the same weighting decision as
  those indicators: hop counts by default, weighted only through `weights=` /
  `weights_as=` (including the `"strength"` inversion), in both `robustness()`
  and `edge_robustness()`. **Results for `radius` on weighted graphs change**
  (e.g. an intact radius of 0.5 from diet proportions becomes the hop-count
  radius, 2). Unweighted graphs are unaffected.
* `optional_robustness_indicators()` gains a `weight_attr` argument (default
  `NA`, unweighted), mirroring `default_robustness_indicators()`. The other
  optional indicators are unchanged: `reciprocity` ignores weights, and
  `mean_topological_diversity` intentionally reads `E(g)$weight` (it is an
  entropy of edge weights).
