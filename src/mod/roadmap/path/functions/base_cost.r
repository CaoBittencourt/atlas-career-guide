# setup
# region: imports
box::use(
  assert = mod / utils / assert,
  mod / roadmap / path / data / graph[...],
  dplyr[...]
)

# endregion
# dispatch
# region: vertex base cost function (starting from nothing)
vertex.cost <- function(vertex, vertices = paths$vertices) {
  # assert args
  stopifnot(round(vertex) == vertex)

  # dispatch
  return(
    vertices[
      vertex,
    ] |>
      select(
        x, t
      ) |>
      sum()
  )
}

# endregion
# exports
# region: exports
box::export(vertex.cost)

# endregion
