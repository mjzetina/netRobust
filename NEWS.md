# netRobust 0.2.0.9000

These changes will be released as netRobust 0.3.0.

## Breaking changes

* `robustness_index()` now follows Eq. 1 of Schneider et al. (2011) exactly:
  it averages X(Q)/X(0) over Q = 1..N, from the first removal to the empty
  network, instead of over Q = 0..N-1. The previous version added exactly
  1/N to the giant-component R-index (e.g. 0.19 instead of the analytic 0.09
  for a 10-vertex star; 0.55 instead of 0.45 for a 10-vertex complete graph).
  **All R-index values change**, and so do the outputs of
  `robustness_index_null()`, `compare_robustness()`,
  `compare_robustness_index_to_null()` and `plot_r_index_null()`. For
  indicators defined at every step (e.g. `giant_component_size`,
  `connectivity`) observed and null values shift by the same amount, so
  p-values are unchanged; for indicators with undefined steps (e.g.
  `mean_distance`, `clustering`) p-values can change slightly. Re-run
  analyses whose R-index values are reported.
* `robustness()` and `edge_robustness()` now return the full curve, from the
  intact network to the empty one (one row more than before), and the step
  column is renamed from `N` (where `N = 1` meant nothing removed) to
  `n_removed` (`0` = intact network), matching `knockout_robustness()`.
  `summarise_null()` and `compare_to_null()` use `n_removed` too. Curves
  computed with earlier versions must be recomputed; `robustness_index()`
  stops with a clear message if given one.

## Bug fixes

* `plot_robustness()`'s x axis, labelled "Number of removed nodes", was off
  by one (it plotted `N`, which started at 1 for the intact network). It now
  plots `n_removed`.

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
