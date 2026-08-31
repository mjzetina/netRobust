#### PACKAGE DATA ####
# Manuel J. Zetina-Rejon
# Bundled example network(s) -- see data-raw/ for how each was built.

#' Zachary's Karate Club network
#'
#' The friendship network among 34 members of a university karate club,
#' recorded by Wayne W. Zachary before the club split into two factions in
#' 1977 -- one of the most widely used example networks in network science,
#' and the network used throughout `vignette("netRobust")`.
#'
#' Regenerated from [igraphdata::karate] (see `data-raw/karate.R`), with
#' [igraph::upgrade_graph()] applied once at data-prep time so loading it
#' here never prints an upgrade message. `igraphdata` itself is not a
#' dependency of netRobust -- only `data-raw/karate.R` needs it, to rebuild
#' this object if it's ever regenerated.
#'
#' @format An `igraph` object: 34 vertices, 78 edges, undirected, weighted.
#' \describe{
#'   \item{`V(karate)$name`}{Member name -- `"Mr Hi"` (the instructor) and
#'     `"John A"` (the club president) by name; everyone else as
#'     `"Actor <n>"`.}
#'   \item{`V(karate)$Faction`}{`1` or `2`: which of the two factions each
#'     member joined after the club's real 1977 split -- the ground truth
#'     the robustness/knock-out/Shapley analyses in the vignette are
#'     checked against.}
#'   \item{`E(karate)$weight`}{Interaction strength (`1`-`7`) between the
#'     two members, as originally recorded by Zachary.}
#' }
#'
#' @source Zachary, W. W. (1977). An Information Flow Model for Conflict
#'   and Fission in Small Groups. *Journal of Anthropological Research*,
#'   33(4), 452-473. Via [igraphdata::karate].
#'
#' @examples
#' igraph::vcount(karate)
#' igraph::is_weighted(karate)
#' table(igraph::V(karate)$Faction)
"karate"
