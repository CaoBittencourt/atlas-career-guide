# setup
# region: imports
box::use(
  gr = igraph,
  roadmap / path / data[...],
  roadmap / path / functions / path_cost[...],
  roadmap / path / functions / which_path[...],
)

# endregion
# dispatch
# region: get path timeline
path.timeline <- function(epath, graph = paths$expected$graph) {
  # assert args in main function
  years <- c(0, path.cost(epath, graph))

  return(
    data.frame(
      year = years |> cumsum(),
      duration = years,
      movement.type = c(
        "start",
        graph |> gr$get.edge.attribute("type", epath)
      ) |>
        as.factor(),
      occupation = epath |> which.path(graph = graph)
    )
  )
}

# endregion
# exports
# region: exports
box::export(path.timeline)

# endregion
