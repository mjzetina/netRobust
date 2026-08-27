#### ROBUSTNESS ####
# Network robustness under sequential node removal
# Manuel J. Zetina-Rejon
# Rewritten Aug-2026: single subgraph per step + pluggable indicator list
#
# INPUT
#   g          : igraph object to analyze
#   ids        : named removal vector (output of random_failures / attack_by_*),
#                names(ids) must be vertex names, in removal order
#   indicators : named list of function(graph) -> scalar. Defaults to
#                default_robustness_indicators(). To add/remove an index
#                for a SINGLE call, prefer exclude=/custom= below; pass
#                indicators= directly only if you want to replace the
#                whole default set.
#   exclude    : character vector of default indicator names to drop
#                (e.g. exclude = c("Homophyly", "Density")). "Connectivity"
#                is a valid name here too -- it's a regular entry in the
#                default indicator set, not a special always-on column.
#                Ignored if you also pass your own indicators=.
#   keep       : character vector of default indicator names to keep --
#                the opposite of exclude= (e.g. keep = c("GC", "APL") to
#                compute only those two; note this also drops
#                "Connectivity" unless you list it explicitly). Ignored if
#                you also pass your own indicators=. If both keep= and
#                exclude= are given, keep= is applied first.
#   custom     : named list of extra function(graph) -> scalar indicators
#                to add on top of the defaults (e.g. Radius, Reciprocity,
#                Diversity -- see examples below). Ignored if you also
#                pass your own indicators=.
#   optional   : shortcut for adding entries from optional_robustness_
#                indicators(g) without calling it yourself and repeating g.
#                TRUE adds every optional indicator that applies to g;
#                a character vector (e.g. c("Radius", "Reciprocity"))
#                adds just those (an unknown name errors; a valid name
#                that doesn't apply to g is dropped with its own warning).
#                Internally forwards to optional_robustness_indicators()'s
#                names= argument, so requesting a subset never triggers
#                the applicability warning of an indicator you didn't ask
#                for. Ignored if you also pass your own indicators=.
#   directed   : should Diameter / APL / Centralization / Homophyly treat
#                the network as directed? Applied consistently across all
#                of them (the original code mixed TRUE and FALSE). NULL
#                (default) auto-detects from g via is_directed(). Passing
#                directed = TRUE explicitly on a graph that isn't actually
#                directed has no effect and triggers a warning.
#   component_mode : "weak"/"strong" for the GC indicator (see robustness.R
#                notes). Only relevant on directed graphs.
#   mode       : "all"/"out"/"in" for distance-based Connectivity. NULL
#                (default) picks "out" when directed, "all" otherwise,
#                matching the directed/undirected treatment used for
#                Diameter/APL so all distance-based indicators agree.
#   weights    : optional explicit numeric vector of edge weights (one per
#                edge, length ecount(g)). If supplied, always wins over
#                weights_as/E(g)$weight -- same override pattern as
#                resolve_weights() used by the attack_by_*() functions.
#   weights_as : how Diameter/APL/Connectivity interpret E(g)$weight when
#                weights = NULL. NULL (default) ignores weights entirely
#                and works on unweighted topology (hop counts) -- the same
#                purely structural treatment used throughout Albert et al.
#                2000, and the default most users analysing network
#                topology (rather than flow/interaction strength) will
#                want -- unlike attack_by_*()'s weights_as, which defaults
#                to each measure's native interpretation instead. Passing
#                weights_as on an unweighted graph (with no weights vector
#                supplied) has no effect and triggers a warning.
#                "distance": use weights as-is (lower = closer).
#                "strength": invert (1/weight) so stronger ties count as
#                closer -- same convention as attack_by_betweenness() /
#                attack_by_closeness().
#
# OUTPUT
#   data.frame, one row per removal step (N = 1 .. vcount(g)), with one
#   column per indicator plus Connectivity (normalised to the intact graph).

# --- Default indicator set -------------------------------------------------
# directed        : treat the network as directed? Only matters for graphs
#                    that ARE directed -- igraph ignores this argument
#                    automatically on undirected graphs, so it's safe to
#                    leave at its auto-detected default in every case.
# component_mode  : "weak" (ignore direction) or "strong" (mutual
#                    reachability required) for the GC indicator. Only
#                    relevant for directed graphs (governance hierarchies,
#                    trophic flows...); ignored on undirected ones.
# weight_attr     : name of an edge attribute to use as weights for
#                    Diameter/APL, or NA (default) for unweighted hop
#                    counts. Deliberately an ATTRIBUTE NAME, not a numeric
#                    vector: these indicator functions get called on gi
#                    (the network as it shrinks during removal), and a
#                    static numeric vector sized for the original graph
#                    would misalign -- or error outright -- once edges
#                    start disappearing along with removed nodes. Reading
#                    the weight fresh from whichever graph is passed in
#                    keeps it correctly aligned at every step, the same way
#                    deleting vertices by name (not index) does for nodes.
#'
#' Default robustness indicator set
#'
#' Builds the named list of `function(graph) -> scalar` indicators used by
#' [robustness()] when `indicators = NULL`: Diameter, GC (giant component
#' size), APL (average path length), Density, Centralization, Clustering,
#' and Homophyly.
#'
#' @param directed Treat the network as directed? Only matters for graphs
#'   that ARE directed -- igraph ignores this argument automatically on
#'   undirected graphs, so it's safe to leave at its auto-detected default.
#' @param component_mode `"weak"` (ignore direction) or `"strong"` (mutual
#'   reachability required) for the `GC` indicator. Only relevant for
#'   directed graphs; ignored on undirected ones.
#' @param weight_attr Name of an edge attribute to use as weights for
#'   Diameter/APL, or `NA` (default) for unweighted hop counts.
#'   Deliberately an attribute name, not a numeric vector: these indicator
#'   functions get called on the network as it shrinks during removal, and
#'   a static numeric vector sized for the original graph would misalign
#'   once edges disappear along with removed nodes.
#'
#' @return A named list of `function(graph) -> scalar`, in the format
#'   expected by [robustness()]'s `indicators=` argument.
#' @seealso [robustness()], [optional_robustness_indicators()]
#' @keywords internal
default_robustness_indicators <- function(directed = FALSE, component_mode = "weak", weight_attr = NA) {
  get_w <- function(gi) if (identical(weight_attr, NA)) NA else igraph::edge_attr(gi, weight_attr)
  list(
    Diameter       = function(g) diameter(g, directed = directed, weights = get_w(g), unconnected = TRUE),
    GC             = function(g) { cs <- components(g, mode = component_mode)$csize; if (length(cs)) max(cs) else 0 },
    APL            = function(g) mean_distance(g, directed = directed, weights = get_w(g), unconnected = TRUE),
    Density        = function(g) edge_density(g, loops = FALSE),
    Centralization = function(g) centr_degree(g, mode = "all", loops = FALSE, normalized = TRUE)$centralization,
    Clustering     = function(g) transitivity(g, type = "global"),
    Homophyly      = function(g) assortativity_degree(g, directed = directed)
  )
}

# --- Optional indicators (opt-in via custom=) -------------------------------
#' Optional robustness indicators with built-in applicability guards
#'
#' Ready-to-use extra indicators for [robustness()]'s `custom=` argument.
#' Not included by default because each only applies to certain network
#' types. Each function checks its own applicability at call time and, if
#' misused, emits an explicit `warning()` and returns `NA` instead of
#' silently returning a misleading value.
#'
#' @param g Optional `igraph` object. If supplied, applicability is checked
#'   ONCE, right here, against `g`: an inapplicable indicator (e.g.
#'   `Reciprocity` on an undirected graph, or `Mean_topological_diversity`
#'   on an unweighted graph) is dropped from the returned list entirely,
#'   with a single warning, instead of being included as a function that
#'   would re-check `is_directed()`/`is_weighted()` (and re-warn) on every
#'   one of the [robustness()] removal steps. Since directedness/
#'   weightedness don't change as nodes are removed, that per-step
#'   re-checking is pure repetition -- with a 25-node network that's the
#'   same warning 26 times. `Mean_topological_diversity` on a directed
#'   graph is similar but different: it's still applicable (it collapses
#'   the graph to undirected internally, per Eagle et al. 2010), but
#'   supplying `g` here still matters -- it moves the "this will collapse
#'   direction" warning to fire once, up front, instead of once per removal
#'   step. If `g = NULL` (default), no pre-filtering happens and every
#'   indicator keeps its own per-call guard, exactly as before.
#' @param names Optional character vector restricting which indicators to
#'   build, e.g. `c("Radius", "Reciprocity")`. Applied *before* the
#'   applicability checks above, so an indicator you didn't ask for never
#'   gets built (and never triggers its warning) just because it happened
#'   to be inapplicable to `g` -- e.g. requesting only `"Radius"` on a
#'   directed graph won't emit `Mean_topological_diversity`'s "will
#'   collapse" warning, since that indicator was never requested in the
#'   first place. An unrecognised name errors. `NULL` (default) builds all
#'   of them.
#'
#' @return A named list of `function(graph) -> scalar`, matching the format
#'   expected by [robustness()]'s `indicators=`/`custom=` arguments.
#'
#' @examples
#' \dontrun{
#' # Old behaviour: guards re-checked (and re-warn) on every removal step
#' robustness(g, ids, custom = optional_robustness_indicators())
#'
#' # Preferred: applicability checked once, against the intact network
#' robustness(g, ids, custom = optional_robustness_indicators(g))
#'
#' # Add just one -- via names=, not by subsetting the returned list, so
#' # unrequested indicators never fire their applicability warnings
#' robustness(g, ids, custom = optional_robustness_indicators(g, names = "Radius"))
#' }
#' @seealso [robustness()], [default_robustness_indicators()]
#' @export
optional_robustness_indicators <- function(g = NULL, names = NULL) {
  # Builds the Mean_topological_diversity function. warn_collapse controls
  # whether it warns about collapsing a directed graph on every single call
  # (TRUE -- the only option when g isn't supplied below, since directedness
  # can't be checked in advance) or silently collapses without repeating
  # that warning at every removal step (FALSE -- used when g IS supplied,
  # since the one-time warning further below already told the user this
  # will happen on every step; repeating it per-step is pure noise,
  # especially on a large network with many removal steps).
  make_diversity <- function(warn_collapse) {
    function(g) {
      if (!is_weighted(g)) {
        warning("Mean_topological_diversity (igraph::diversity()) requires a weighted graph; returning NA for this unweighted graph.", call. = FALSE)
        return(NA_real_)
      }
      if (is_directed(g)) {
        if (warn_collapse) {
          warning("Mean_topological_diversity: g is directed; collapsing to ",
                  "undirected (mode=\"collapse\", summing reciprocal edge weights) ",
                  "before computing diversity, following the topological ",
                  "treatment in Eagle et al. (2010) / Zetina-Rejon et al. (2022). ",
                  "Directional information (e.g. predator vs. prey link direction) ",
                  "is discarded for this indicator.", call. = FALSE)
        }
        g <- as_undirected(g, mode = "collapse", edge.attr.comb = list(weight = "sum"))
      }
      # A graph fragmented enough to have lost all its edges has no weights
      # to compute an entropy from -- igraph::diversity() errors hard on
      # this (unlike Diameter/APL's graceful unconnected=TRUE handling of
      # similarly degenerate cases). This is an expected, benign endpoint of
      # node removal, not a real problem, so it returns NA quietly here
      # instead of surfacing as a "such-and-such failed" warning via
      # robustness()'s per-indicator tryCatch.
      if (ecount(g) == 0) return(NA_real_)
      mean(igraph::diversity(g), na.rm = TRUE)
    }
  }

  full <- list(
    Radius = function(g) radius(g, mode = "all"),

    Reciprocity = function(g) {
      if (!is_directed(g)) {
        warning("Reciprocity only applies to directed graphs; returning NA for this undirected graph.", call. = FALSE)
        return(NA_real_)
      }
      reciprocity(g, ignore.loops = TRUE)
    },

    # Mean topological diversity: normalised Shannon entropy of each node's
    # incident edge weights (igraph::diversity()), following Eagle et al.
    # (2010) eq. 3-5 -- H(i) = -sum_j p_ij*log(p_ij), p_ij = V_ij / sum_j
    # V_ij over i's k contacts, normalised by log(k). Same naming/method as
    # "Mean topological diversity" in Zetina-Rejon et al. (2022) and
    # Fumero et al. (2024). Requires a WEIGHTED graph -- igraph's version
    # handles degree-0 vertices (NaN) and degree-1 vertices (0) natively,
    # no manual cleanup needed. Explicitly namespaced (igraph::) because
    # vegan also exports a diversity() with a completely different
    # signature (community data matrix, not an igraph object) -- if vegan
    # is loaded after igraph (as it is in main.R), an unqualified
    # diversity(g) call resolves to vegan's version and fails.
    #
    # Eagle et al.'s original formula operates on a single, undirected set
    # of "i's contacts" (phone communication volume, inherently mutual) --
    # it doesn't distinguish outgoing/incoming ties. On a directed graph
    # (e.g. a trophic network), this is collapsed to undirected first
    # (mode="collapse", summing reciprocal edge weights) so the formula
    # applies as originally defined -- warned about once (see warn_collapse
    # above / the g-supplied check below), not on every removal step.
    Mean_topological_diversity = make_diversity(warn_collapse = TRUE)
  )

  if (!is.null(names)) {
    unknown <- setdiff(names, base::names(full))
    if (length(unknown) > 0) {
      stop("names = ", paste(unknown, collapse = ", "), " not found. Available ",
           "optional indicators: ", paste(base::names(full), collapse = ", "),
           ".", call. = FALSE)
    }
    full <- full[names]  # narrow BEFORE the applicability checks below, so an
                          # indicator that wasn't requested never triggers its
                          # own applicability warning just for existing
  }

  if (!is.null(g)) {
    if ("Reciprocity" %in% base::names(full) && !is_directed(g)) {
      warning("Reciprocity only applies to directed graphs; dropping it from ",
              "custom indicators for this undirected graph (checked once).", call. = FALSE)
      full$Reciprocity <- NULL
    }
    if ("Mean_topological_diversity" %in% base::names(full)) {
      if (!is_weighted(g)) {
        warning("Mean_topological_diversity (igraph::diversity()) requires a ",
                "weighted graph; dropping it from custom indicators for this ",
                "unweighted graph (checked once).", call. = FALSE)
        full$Mean_topological_diversity <- NULL
      } else if (is_directed(g)) {
        warning("Mean_topological_diversity: g is directed; every removal step ",
                "will collapse it to undirected (mode=\"collapse\", summing ",
                "reciprocal edge weights) before computing diversity, following ",
                "the topological treatment in Eagle et al. (2010) / ",
                "Zetina-Rejon et al. (2022). Directional information (e.g. ",
                "predator vs. prey link direction) is discarded for this ",
                "indicator. (Shown once here instead of on every removal step.)",
                call. = FALSE)
        full$Mean_topological_diversity <- make_diversity(warn_collapse = FALSE)
      }
    }
  }

  full
}

# --- Shared argument/indicator resolution (internal) ------------------------
# The setup logic behind robustness() and edge_robustness() that has nothing
# to do with whether the thing being removed is a vertex or an edge: resolve
# directed/mode, resolve which weights (if any) the distance-based indicators
# use, bake them into g as an edge attribute, compute Connectivity's
# intact-graph baseline, and assemble the final indicators list from
# keep=/exclude=/optional=/custom=. Returns list(g=, indicators=) -- g comes
# back with the baked-in weight attribute (if any) already set; the caller
# still owns unit-specific validation (vertex vs. edge names) and the actual
# .removal_curve() call with the right delete_fn.
.resolve_robustness_setup <- function(g, indicators, exclude, keep, custom, optional,
                                       directed, component_mode, mode, weights, weights_as) {
  stopifnot(inherits(g, "igraph"))
  if (is.null(directed)) {
    directed <- is_directed(g)
  } else if (directed && !is_directed(g)) {
    warning("directed = TRUE was requested, but g is an undirected graph; ",
            "directed has no effect on undirected graphs regardless of its value.")
  }
  if (is.null(mode)) mode <- if (directed) "out" else "all"

  # Resolve which edge weights (if any) to use for the distance-based
  # indicators (Diameter, APL, Connectivity). Unlike resolve_weights() (used
  # by the attack_by_*() functions), the default here is deliberately
  # unweighted/structural even on a weighted graph -- weighting is opt-in
  # via weights_as/weights, not automatic.
  dist_weights <- NA
  if (!is.null(weights)) {
    if (length(weights) != ecount(g)) {
      stop("weights must have one value per edge: length(weights) = ", length(weights),
           ", but ecount(g) = ", ecount(g), ".", call. = FALSE)
    }
    dist_weights <- weights
  } else if (!is.null(weights_as)) {
    if (!is_weighted(g)) {
      warning("weights_as = \"", weights_as[1], "\" has no effect: g is an ",
              "unweighted graph and weights = NULL. Pass an explicit weights ",
              "vector if you want weighting to be applied.")
    } else {
      weights_as <- match.arg(weights_as, choices = c("distance", "strength"))
      dist_weights <- if (weights_as == "distance") E(g)$weight else 1 / E(g)$weight
    }
  }

  # Bake the resolved weights into g as an edge attribute BEFORE the removal
  # loop starts, instead of carrying a plain numeric vector across steps --
  # see default_robustness_indicators()'s weight_attr notes for why a static
  # vector misaligns (or errors) once edges start disappearing along with
  # removed nodes/edges. weight_attr is the attribute name to read per-step,
  # or NA if no weighting was requested.
  weight_attr <- NA
  if (!identical(dist_weights, NA)) {
    weight_attr <- ".robustness_weight"
    g <- set_edge_attr(g, weight_attr, value = dist_weights)
  }
  get_weights <- function(gi) if (identical(weight_attr, NA)) NA else igraph::edge_attr(gi, weight_attr)

  # Reference (intact-graph) reachable-pairs count, to normalise Connectivity.
  # Uses the same mode/weights decision as Diameter/APL, so every
  # distance-based indicator agrees on how to treat direction and weight.
  # Computed here (before building `indicators`) so Connectivity can be
  # added as a regular entry in that list -- subject to keep=/exclude=/
  # custom= like every other indicator, instead of being hardcoded to
  # always compute regardless of what the user asked for.
  dist_full <- distances(g, mode = mode, weights = get_weights(g))
  dist_full[!is.finite(dist_full)] <- 0
  tot_full <- sum(dist_full > 0)

  connectivity_indicator <- function(gi) {
    dist_i <- distances(gi, mode = mode, weights = get_weights(gi))
    dist_i[!is.finite(dist_i)] <- 0
    if (tot_full > 0) sum(dist_i > 0) / tot_full * 100 else NA_real_
  }

  if (is.null(indicators)) {
    indicators <- c(default_robustness_indicators(directed, component_mode, weight_attr),
                     list(Connectivity = connectivity_indicator))
    if (!is.null(keep))    indicators <- indicators[intersect(names(indicators), keep)]
    if (!is.null(exclude)) indicators <- indicators[setdiff(names(indicators), exclude)]
    if (!is.null(optional)) {
      if (!isTRUE(optional) && !is.character(optional)) {
        stop("optional must be TRUE or a character vector of names, e.g. ",
             "optional = c(\"Radius\", \"Reciprocity\").", call. = FALSE)
      }
      # names = NULL (optional=TRUE) builds all of them; a character vector
      # restricts BOTH which get built AND which applicability warnings can
      # fire, so requesting only e.g. "Radius" never triggers
      # Mean_topological_diversity's "will collapse" warning just because
      # it happened to be inapplicable -- it was never requested.
      req_names <- if (isTRUE(optional)) NULL else optional
      indicators <- c(indicators, optional_robustness_indicators(g, names = req_names))
    }
    if (!is.null(custom))  indicators <- c(indicators, custom)
  } else if (!is.list(indicators) || is.null(names(indicators)) ||
             any(names(indicators) == "") || !all(vapply(indicators, is.function, logical(1)))) {
    stop("indicators must be a named list of function(graph) -> scalar, e.g. ",
         "list(GC = function(g) ...). To pick a subset of the default ",
         "indicators instead, use keep=/exclude=, not indicators= -- e.g. ",
         "robustness(g, ids, keep = c(\"GC\", \"APL\")).", call. = FALSE)
  }

  list(g = g, indicators = indicators)
}

# --- Compute one row of indicator values (internal, shared) ----------------
# Applies every indicator function to gi, catching individual failures as NA
# (with a warning naming which one failed) instead of aborting the whole
# simulation over one bad indicator. Shared by .removal_curve() (one call
# per step of a sequential removal) and knockout_robustness() (one call per
# independent group trial) -- both just need "indicator values for this one
# graph state", the surrounding loop differs.
.compute_indicator_row <- function(gi, indicators) {
  Map(function(nm, f) {
    tryCatch(f(gi), error = function(e) {
      warning(nm, " failed and returned NA: ", conditionMessage(e), call. = FALSE)
      NA_real_
    })
  }, names(indicators), indicators)
}

# --- Shared removal-and-compute loop (internal) -----------------------------
# The core simulation loop behind robustness() and edge_robustness(): starting
# from g intact, delete one unit (vertex or edge) at a time -- by NAME,
# robust to igraph's internal re-indexing after each deletion -- recomputing
# every indicator after each step. delete_fn does the one-unit deletion
# (delete_vertices for robustness(), delete_edges for edge_robustness());
# everything specific to which unit is being removed (weight resolution,
# name validation, Connectivity's baseline distance count) is the caller's
# job and stays in robustness()/edge_robustness() respectively, since that's
# where node-vs-edge specifics actually live -- this helper only knows how
# to walk removal_names and build rows.
.removal_curve <- function(g, removal_names, indicators, delete_fn) {
  n <- length(removal_names)
  rows <- vector("list", n + 1)
  rows[[1]] <- c(list(N = 1), .compute_indicator_row(g, indicators))

  gi <- g
  for (i in seq_len(n)) {
    gi <- delete_fn(gi, removal_names[i])
    rows[[i + 1]] <- c(list(N = i + 1), .compute_indicator_row(gi, indicators))
  }

  df <- do.call(rbind.data.frame, rows)
  df <- df[seq_len(n), ]  # drop the fully-emptied network, matches original behaviour
  rownames(df) <- NULL
  df
}

# --- Indicator lookup table --------------------------------------------
#' List all robustness indicators available to robustness()
#'
#' A lookup table describing every indicator [robustness()] knows about:
#' the built-in defaults (always computed unless dropped via `exclude=`/
#' `keep=`) and the opt-in `optional`/`custom=` set from
#' [optional_robustness_indicators()], plus what each one requires of the
#' network, if anything. This is the single source of truth for indicator
#' descriptions -- `?robustness`'s "Available indicators" section lists
#' just the names, and points here for detail, so the two can't drift out
#' of sync.
#'
#' @return A `data.frame` with columns `name`, `type` (`"default"` or
#'   `"optional"`), `description`, and `requires` (`"directed"`,
#'   `"weighted"`, or `NA` if the indicator has no such restriction).
#'
#' @examples
#' list_indicators()
#' list_indicators()[list_indicators()$type == "optional", ]
#'
#' @seealso [robustness()], [default_robustness_indicators()],
#'   [optional_robustness_indicators()]
#' @export
list_indicators <- function() {
  data.frame(
    name = c(
      "Diameter", "GC", "APL", "Density", "Centralization", "Clustering",
      "Homophyly", "Connectivity",
      "Radius", "Reciprocity", "Mean_topological_diversity"
    ),
    type = c(rep("default", 8L), rep("optional", 3L)),
    description = c(
      "Longest shortest path between any two reachable vertices (hop count, or weighted distance via weights_as=).",
      "Size of the giant (largest) connected component.",
      "Average shortest-path length across all reachable vertex pairs.",
      "Proportion of possible edges that are actually present.",
      "Freeman degree centralization: how unevenly degree is distributed across vertices (0 = regular graph, 1 = star).",
      "Global transitivity: proportion of connected triples that form triangles.",
      "Degree assortativity: do high-degree vertices preferentially connect to other high-degree vertices?",
      "Giant component size, normalised to the intact network's reachable pairs (0-100%).",
      "Minimum eccentricity: shortest path length from the most central vertex to its farthest vertex.",
      "Proportion of edges that are reciprocated (mutual).",
      "Mean normalised Shannon entropy of each vertex's incident edge weights (Eagle et al. 2010)."
    ),
    requires = c(rep(NA_character_, 9L), "directed", "weighted"),
    stringsAsFactors = FALSE
  )
}

#' Network robustness under sequential node removal
#'
#' Removes vertices from `g` one at a time, in the order given by `ids`
#' (typically the output of [random_failures()] or an `attack_by_*()`
#' function), recomputing a set of structural indicators after each
#' removal. This is the core simulation function of the package: pass it a
#' failure or attack order and it traces out the resulting robustness
#' curve, one row per number of nodes removed.
#'
#' @param g An `igraph` object to analyze.
#' @param ids Named removal vector (output of [random_failures()] or an
#'   `attack_by_*()` function). `names(ids)` must be vertex names, in
#'   removal order.
#' @param indicators Named list of `function(graph) -> scalar`. Defaults to
#'   [default_robustness_indicators()] (plus `Connectivity`). To add/remove
#'   an index for a single call, prefer `exclude=`/`keep=`/`custom=` below;
#'   pass `indicators=` directly only if you want to replace the whole
#'   default set.
#' @param exclude Character vector of default indicator names to drop
#'   (e.g. `exclude = c("Homophyly", "Density")`). `"Connectivity"` is a
#'   valid name here too -- it's a regular entry in the default indicator
#'   set, not a special always-on column. Ignored if you also pass your
#'   own `indicators=`.
#' @param keep Character vector of default indicator names to keep -- the
#'   opposite of `exclude=` (e.g. `keep = c("GC", "APL")` to compute only
#'   those two; note this also drops `"Connectivity"` unless you list it
#'   explicitly). Ignored if you also pass your own `indicators=`. If both
#'   `keep=` and `exclude=` are given, `keep=` is applied first.
#' @param custom Named list of extra `function(graph) -> scalar` indicators
#'   to add on top of the defaults (e.g. Radius, Reciprocity, Diversity --
#'   see [optional_robustness_indicators()]). Ignored if you also pass your
#'   own `indicators=`.
#' @param optional Shortcut for adding entries from
#'   [optional_robustness_indicators()] without calling it yourself and
#'   repeating `g`. `TRUE` adds every optional indicator that applies to
#'   `g`; a character vector (e.g. `c("Radius", "Reciprocity")`) adds just
#'   those (an unknown name errors; a valid name that doesn't apply to `g`
#'   is dropped with its own warning). Internally forwards to
#'   [optional_robustness_indicators()]'s `names=` argument, so requesting
#'   a subset never triggers the applicability warning of an indicator you
#'   didn't ask for. Ignored if you also pass your own `indicators=`.
#' @param directed Should Diameter / APL / Centralization / Homophyly treat
#'   the network as directed? Applied consistently across all of them.
#'   `NULL` (default) auto-detects from `g` via `is_directed()`. Passing
#'   `directed = TRUE` explicitly on a graph that isn't actually directed
#'   has no effect and triggers a warning.
#' @param component_mode `"weak"`/`"strong"` for the `GC` indicator. Only
#'   relevant on directed graphs: `"weak"` ignores direction when defining
#'   a connected component; `"strong"` requires mutual reachability.
#' @param mode `"all"`/`"out"`/`"in"` for distance-based `Connectivity`.
#'   `NULL` (default) picks `"out"` when directed, `"all"` otherwise,
#'   matching the directed/undirected treatment used for Diameter/APL so
#'   all distance-based indicators agree.
#' @param weights Optional explicit numeric vector of edge weights (one per
#'   edge, length `ecount(g)`). If supplied, always wins over
#'   `weights_as`/`E(g)$weight` -- same override pattern as
#'   [resolve_weights()], used by the `attack_by_*()` functions.
#' @param weights_as How Diameter/APL/Connectivity interpret `E(g)$weight`
#'   when `weights = NULL`. `NULL` (default) ignores weights entirely and
#'   works on unweighted topology (hop counts) -- the same purely
#'   structural treatment used throughout Albert et al. (2000), and the
#'   default most users analysing network topology (rather than
#'   flow/interaction strength) will want -- unlike `attack_by_*()`'s
#'   `weights_as`, which defaults to each measure's native interpretation
#'   instead. Passing `weights_as` on an unweighted graph (with no
#'   `weights` vector supplied) has no effect and triggers a warning.
#'   `"distance"`: use weights as-is (lower = closer). `"strength"`:
#'   invert (`1/weight`) so stronger ties count as closer -- same
#'   convention as [attack_by_betweenness()]/[attack_by_closeness()].
#'
#' @return A `data.frame`, one row per removal step (`N` = 1 ..
#'   `vcount(g)`), with one column per indicator plus `Connectivity`
#'   (normalised to the intact graph).
#'
#' @section Available indicators:
#' Default (always computed unless dropped via `exclude=`/`keep=`):
#' `Diameter`, `GC`, `APL`, `Density`, `Centralization`, `Clustering`,
#' `Homophyly`, `Connectivity`. Optional (opt-in via `custom=`/`optional=`):
#' `Radius`, `Reciprocity` (directed graphs only), `Mean_topological_diversity`
#' (weighted graphs only). See [list_indicators()] for what each one
#' measures.
#'
#' @seealso [random_failures()], [attack_by_degree()],
#'   [default_robustness_indicators()], [optional_robustness_indicators()],
#'   [list_indicators()], [robustness_null_removal()]
#' @export
robustness <- function(g, ids, indicators = NULL, exclude = NULL, keep = NULL,
                        custom = NULL, optional = NULL,
                        directed = NULL, component_mode = "weak", mode = NULL,
                        weights = NULL, weights_as = NULL) {
  setup <- .resolve_robustness_setup(g, indicators, exclude, keep, custom, optional,
                                      directed, component_mode, mode, weights, weights_as)
  g <- setup$g

  if (is.null(V(g)$name)) V(g)$name <- paste0("node_", seq_len(vcount(g)))

  n <- vcount(g)
  removal_names <- names(ids)[seq_len(n)]
  stopifnot(all(removal_names %in% V(g)$name))

  # Delete one vertex at a time from the already-shrunk graph, by NAME
  # (robust to igraph's internal re-indexing after each deletion) -- see
  # .removal_curve() above.
  .removal_curve(g, removal_names, setup$indicators, delete_vertices)
}
