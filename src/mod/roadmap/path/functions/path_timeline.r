# setup
# region: imports
box::use(
  gr = igraph,
  roadmap / path / data / graph[...],
  roadmap / path / functions / path_cost[...],
  roadmap / path / functions / which_path[...],
)

# endregion
# dispatch
# region: get path timeline
path.timeline <- function(epath, graph = paths$graph) {
  # assert args in main function
  years <- c(0, path.cost(epath, graph))

  return(
    data.frame(
      year = years |> cumsum(),
      duration = years,
      movement.type = c("start", graph |> gr$get.edge.attribute("type", epath)) |> as.factor(),
      occupation = epath |> which.path()
    )
  )
}

# endregion
# exports
# region: exports
box::export(path.timeline)

# endregion
