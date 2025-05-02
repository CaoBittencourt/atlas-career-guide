modular::project.options("atlas")
# setup
# region: imports
box::use(
  assert = mod / utils / assert,
  mod/roadmap/path/data/vertices[...]
)

# endregion
# methods
# region: min method
which.vertex.min <- function() {
  # assert args in main function
  # description
  return("min")
}

# endregion
# region: max method
which.vertex.max <- function() {
  # assert args in main function
  # description
  return("max")
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
  assert$base$validate.method(which.vertex_method, "which.vertex_method", which.vertex.method)


  # multiple dispatch
  if (which.vertex_method[[1]] == which.vertex.methods$min) {
    return(which.vertex.min())
  }

  if (which.vertex_method[[1]] == which.vertex.methods$max) {
    return(which.vertex.max())
  }
}

# endregion
# exports
# region: exports
box::export(which.vertex, which.vertex.methods)

# endregion
