# netRobust

Simulate and compare network robustness to random node failures and
targeted attacks (by degree, betweenness, closeness, eigenvector
centrality, and PageRank), extending the approach of Albert, Jeong &
Barabási (2000). Includes a Monte Carlo framework for testing whether an
attack scenario is statistically more damaging than random failure, and
utilities for plotting robustness curves with a null-model reference
envelope.

Works on any `igraph` object -- directed or undirected, weighted or not --
not just trophic/food-web networks: social, governance, value-chain, or
any other network where you want to ask "how much does removing nodes
break this?"

## Installation

Not on CRAN yet. Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("mjzetina/netRobust")
```

## Quick example

```r
library(netRobust)
library(igraph)

g <- make_graph("Zachary")            # Zachary's Karate Club, 34 nodes

r <- random_failures(g)               # random removal order
d <- attack_by_degree(g)              # remove highest-degree nodes first

failure <- robustness(g, ids = r, keep = c("GC", "APL", "Connectivity"))
attack  <- robustness(g, ids = d, keep = c("GC", "APL", "Connectivity"))

# Is the attack significantly worse than random failure, overall?
null_dist <- robustness_null_removal(g, n_reps = 200, seed = 1,
                                      keep = c("GC", "APL", "Connectivity"))
compare_robustness_index_to_null(attack, null_dist, index = "GC")
```

## Learn more

```r
vignette("netRobust", package = "netRobust")
```

walks through the full workflow end to end -- removal orders, robustness
curves, null models (both the classic random-failure null and a
structural/degree-preserving one), plotting, and the statistical
comparisons (pointwise and whole-curve). Every function also has its own
`?` help page; browse them all with `help(package = "netRobust")`.

## Citation

This package generalises two published methods:

- Albert, R., Jeong, H., & Barabási, A.-L. (2000). Error and attack
  tolerance of complex networks. *Nature*, 406, 378-382.
  <doi:10.1038/35019019>
- Schneider, C. M., Moreira, A. A., Andrade, J. S., Havlin, S., &
  Herrmann, H. J. (2011). Mitigation of malicious attacks on networks.
  *Proceedings of the National Academy of Sciences*, 108(10), 3838-3841.
  <doi:10.1073/pnas.1009440108>

## License

MIT © Manuel J. Zetina-Rejón
