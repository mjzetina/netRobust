# netRobust 0.2.0.9000

## New features

* Every centrality-based attack -- `attack_by_degree()`,
  `attack_by_betweenness()`, `attack_by_closeness()`,
  `attack_by_eigenvector()`, `attack_by_pagerank()`, `attack_by_hits()` and
  `attack_by_edge_betweenness()` -- gains `recalculate = FALSE`. With `TRUE`
  the centrality is recomputed on the remaining network after every removal
  (an adaptive attack, Holme et al. 2002), as in NetworkExtinction,
  `bipartite::second.extinct(method = "degree")` and NetSwan. The output has
  the same format as the static order, so it works unchanged in
  `robustness()`, `edge_robustness()` and `robustness_null_structural()`.
  See `?recalculated_attacks` for the computational cost and for why a
  recalculated HITS attack needs `set.seed()` to be reproducible.
  `attack_by_edge_weight()` has no such option, since edge weights do not
  change as other edges are removed.

## Citation

* `citation("netRobust")`, the README and `CITATION.cff` now give the Zenodo
  DOI of the 0.2.0 release (10.5281/zenodo.23330079).

# netRobust 0.2.0

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

## Documentation

* The vignette no longer claims that every `attack_by_*()` function has a
  `mode` argument (only `attack_by_degree()` and `attack_by_closeness()` do),
  and now explains that `weights_as` inverts weights relative to each
  measure's native reading: distance for betweenness/closeness, strength for
  eigenvector/PageRank/HITS.

## Citation and package metadata

* New `CITATION.cff`; each GitHub release is archived on Zenodo with a DOI.
  The README explains how to cite the package.
* DESCRIPTION gains `URL`, `BugReports` and the maintainer's ORCID; the
  maintainer address is now the institutional one.
