# Regenerates data/karate.rda from igraphdata::karate.
#
# igraphdata is NOT a dependency of netRobust -- it's only needed to run
# this script once, when the bundled dataset needs to be rebuilt (e.g. after
# an igraph format change). Run interactively:
#   install.packages("igraphdata")  # if not already installed
#   source("data-raw/karate.R")

if (!requireNamespace("igraphdata", quietly = TRUE)) {
  stop("Install igraphdata to regenerate data/karate.rda: install.packages(\"igraphdata\")")
}

data("karate", package = "igraphdata", envir = environment())

# igraphdata's copy was created by an older igraph version -- upgrade it
# once here, at data-prep time, instead of on every load by every user.
# Graph-level attributes (Citation, Author) are preserved automatically.
karate <- igraph::upgrade_graph(karate)

usethis::use_data(karate, overwrite = TRUE)
