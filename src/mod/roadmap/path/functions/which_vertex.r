# setup
# region: imports
box::use(
  assert = mod / utils / assert,
  mod / roadmap / path / data / vertices[...],
  dplyr[...]
)

# endregion
# methods
# region: min method
which.vertex.min <- function(id) {
  # assert args in main function
  # get occupation's vertex with min requirements
  return(
    vertices |>
      filter(
        occupation == id
      ) |>
      arrange(x, t) |>
      slice(1) |>
      pull(vertex)
  )
}

# endregion
# region: max method
which.vertex.max <- function(id) {
  # assert args in main function
  # get occupation's vertex with max requirements
  return(
    vertices |>
      filter(
        occupation == id
      ) |>
      arrange(-x, -t) |>
      slice(1) |>
      pull(vertex)
  )
}

# endregion
# region: list of methods
list(
  "min" = "min",
  "max" = "max"
) -> which.vertex.methods

# endregion
# dispatch
# region: which.vertex generic function
which.vertex <- function(occupation, which.vertex_method = which.vertex.methods[[1]], ...) {
  # assert args
  stopifnot(is.integer(occupation))
  assert$base$validate.method(which.vertex_method, "which.vertex_method", which.vertex.methods)

  # multiple dispatch
  if (which.vertex_method[[1]] == which.vertex.methods$min) {
    return(which.vertex.min(occupation))
  }

  if (which.vertex_method[[1]] == which.vertex.methods$max) {
    return(which.vertex.max(occupation))
  }
}

# endregion
# exports
# region: exports
box::export(which.vertex, which.vertex.methods)

# endregion
