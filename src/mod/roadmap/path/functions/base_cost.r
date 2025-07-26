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
  vertices |>
    filter(
      career == 874
    ) |>
    slice(1) |>
    pull(vertex) -> basic.education.id
  # dispatch
  return(
    vertex |>
      vapply(
        function(q) {
          basic.education.id |>
            path(q) |>
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
# region: vertex base cost with expected graph
vertex.cost.expected <- function(vertex) {
  # assert args in main function
  # dispatch
  return(
    vertex.cost(
      vertex,
      graph = paths$expected$graph,
      vertices = paths$expected$vertices
    )
  )
}

# endregion
# region: vertex base cost with detailed graph
vertex.cost.detailed <- function(vertex) {
  # assert args in main function
  # dispatch
  return(
    vertex.cost(
      vertex,
      graph = paths$detailed$graph,
      vertices = paths$detailed$vertices
    )
  )
}

# endregion
# exports
# region: exports
box::export(
  vertex.cost,
  vertex.cost.expected,
  vertex.cost.detailed
)

# endregion
