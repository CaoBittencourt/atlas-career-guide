# setup
# region: imports
box::use(
  assert = utils / assert,
  roadmap / path / data[...],
  roadmap / path / functions / path_cost[...],
  roadmap / path / path[...],
  dplyr[...]
)

# endregion
# dispatch
# region: vertex base cost function (starting from nothing)
vertex.cost <- function(
  vertex,
  graph = paths$expected$graph,
  vertices = paths$expected$vertices
) {
  # assert args
  stopifnot(round(vertex) == vertex)

  # dispatch
  return(
    vertex |>
      vapply(
        function(q) {
          q |>
            path(
              vertices |>
                filter(
                  career == 874
                ) |>
                slice(1) |>
                pull(vertex)
            ) |>
            path.cost() |>
            sum()
        },
        FUN.VALUE = numeric(1)
      )
    # graph |>
    #   gr$get.vertex.attribute(
    #     "base.cost",
    #     vertex
    #   )
  )
}

# endregion
# exports
# region: exports
box::export(vertex.cost)

# endregion
