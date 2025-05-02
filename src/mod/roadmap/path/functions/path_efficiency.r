# setup
# region: imports
box::use(
  gr = igraph,
  mod / utils / data / last[...],
  mod / roadmap / path / data / graph[...],
  mod / roadmap / path / functions / path_cost[...],
  mod / roadmap / path / functions / base_cost[...],
)

# endregion
# dispatch
# region: get path efficiency (as a percentage of base cost)
path.efficiency <- function(epath, graph = paths$graph, vertices = paths$vertices) {
  # assert args in main function
  return(
    1 - (
      (
        epath |> path.cost(graph) |> sum()
      ) / (
        graph |>
          gr$get.edge.attribute(
            "occupation.to",
            epath
          ) |>
          last() |>
          vertex.cost(
            vertices
          )
      )
    )
  )
}

# endregion
# exports
# region: exports
box::export(path.efficiency)

# endregion
