# setup
# region: imports
box::use(
  gr = igraph,
  roadmap / path / data / graph[...],
)

# endregion
# dispatch
# region: get path cost (in years)
path.cost <- function(epath, graph = paths$graph) {
  # assert args in main function
  return(
    graph |>
      gr$get.edge.attribute(
        "cost",
        epath
      )
  )
}

# endregion
# exports
# region: exports
box::export(path.cost)

# endregion
