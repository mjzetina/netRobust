#### KNOCKOUT ROBUSTNESS ####

# --- Normalize the `groups=` argument (internal) ----------------------------
# Accepts NULL, a bare vector (shorthand for a single group), or a list of
# vectors -- each element either character (vertex names) or numeric
# (vertex indices), independently per group (not mixed within one group's
# own vector). Returns a canonical list of deduplicated character vectors
# (vertex names) -- one warning naming which group_id(s) had duplicates
# removed, if any; one error naming an out-of-range index or unrecognised
# name, since that's invalid vocabulary, not a contradiction to warn about.
.normalize_groups <- function(groups, g) {
  if (is.null(groups)) {
    groups <- as.list(V(g)$name)
  } else if (!is.list(groups)) {
    groups <- list(groups)
  }

  resolve_one <- function(grp, i) {
    if (is.numeric(grp)) {
      bad <- grp[grp < 1 | grp > vcount(g) | grp != round(grp)]
      if (length(bad) > 0) {
        stop("groups[[", i, "]] has out-of-range vertex index/indices: ",
             paste(unique(bad), collapse = ", "), ". Valid range is 1..",
             vcount(g), ".", call. = FALSE)
      }
      resolved <- V(g)$name[grp]
    } else if (is.character(grp)) {
      bad <- setdiff(grp, V(g)$name)
      if (length(bad) > 0) {
        stop("groups[[", i, "]] has vertex name(s) not found in g: ",
             paste(bad, collapse = ", "), ".", call. = FALSE)
      }
      resolved <- grp
    } else {
      stop("Each element of groups must be a character vector (vertex ",
           "names) or a numeric vector (vertex indices); groups[[", i,
           "]] is a ", paste(class(grp), collapse = "/"), ".", call. = FALSE)
    }
    if (length(resolved) == 0) {
      stop("groups[[", i, "]] is empty; each group must contain at least ",
           "one vertex.", call. = FALSE)
    }
    resolved
  }

  resolved <- Map(resolve_one, groups, seq_along(groups))

  dup_ids <- which(vapply(resolved, function(x) anyDuplicated(x) > 0, logical(1)))
  if (length(dup_ids) > 0) {
    warning(length(dup_ids), " group(s) had duplicate vertex names/indices, ",
            "deduplicated: group_id ", paste(dup_ids, collapse = ", "), ".",
            call. = FALSE)
  }
  unname(lapply(resolved, unique))
}

# --- Isolated-removal ("knock-out") robustness ------------------------------
#' Isolated-removal ("knock-out") robustness
#'
#' Tests the isolated effect of removing one or more vertices at a time,
#' independently -- unlike [robustness()]'s cumulative, sequential removal,
#' every group here is tested against the *intact* network and then
#' discarded, never against the result of a previous group's removal.
#' "What happens if this species/actor disappears, on its own?" generalised
#' to "...if this *set* of species/actors disappears together, on its
#' own?".
#'
#' @param g An `igraph` object to analyze.
#' @param groups What to knock out, one group per trial. `NULL` (default)
#'   tests every vertex individually, one group per vertex -- the classic
#'   single-node knock-out. Otherwise a list, one element per group, each
#'   element either a character vector of vertex names or a numeric vector
#'   of vertex indices (the two can be mixed *across* different groups,
#'   just not within the same group's own vector). A bare (non-list)
#'   vector is shorthand for a single group, e.g. `groups = c("A", "B")`
#'   or `groups = c(1, 2)`. Duplicate names/indices within one group are
#'   silently deduplicated, with a single warning naming which
#'   `group_id`(s) were affected. An out-of-range index or an unrecognised
#'   name errors instead -- that's invalid input, not something to guess
#'   at silently.
#' @param indicators,exclude,keep,custom,optional,directed,component_mode,mode,weights,weights_as
#'   As in [robustness()].
#'
#' @return A `data.frame`, one row per group, with columns:
#'   \describe{
#'     \item{`group_id`}{Integer, `1:length(groups)`, identifying each
#'       trial in the order given (or vertex order, if `groups = NULL`).}
#'     \item{`group`}{List-column: the exact, deduplicated vertex names
#'       removed in that trial.}
#'     \item{`n_removed`}{Group size, after deduplication.}
#'   }
#'   plus one column per indicator.
#'
#' @seealso [robustness()], [edge_robustness()], [list_indicators()]
#' @export
knockout_robustness <- function(g, groups = NULL, indicators = NULL, exclude = NULL, keep = NULL,
                                 custom = NULL, optional = NULL,
                                 directed = NULL, component_mode = "weak", mode = NULL,
                                 weights = NULL, weights_as = NULL) {
  setup <- .resolve_robustness_setup(g, indicators, exclude, keep, custom, optional,
                                      directed, component_mode, mode, weights, weights_as)
  g <- setup$g
  if (is.null(V(g)$name)) V(g)$name <- paste0("node_", seq_len(vcount(g)))

  groups <- .normalize_groups(groups, g)

  rows <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    gi <- delete_vertices(g, groups[[i]])
    rows[[i]] <- c(list(group_id = i, n_removed = length(groups[[i]])),
                    .compute_indicator_row(gi, setup$indicators))
  }

  df <- do.call(rbind.data.frame, rows)
  df$group <- groups
  rownames(df) <- NULL
  df[, c("group_id", "group", "n_removed", setdiff(names(df), c("group_id", "group", "n_removed")))]
}
