# setup
# region: imports
box::use(
  assert = mod / utils / assert,
)

# endregion
# methods
# region: default method
employment.default <- function(w, x, t, x.pct, t.pct) {
  # assert args in main function
  # uniform distribution
  return(1)
}

# endregion
# region: list of methods
list(
  "default" = "default"
) -> employment.methods

# endregion
# dispatch
# region: employment generic function
employment <- function(employment_method = employment.methods[[1]], ...) {
  # assert args

  # multiple dispatch
  if (employment_method[[1]] == employment.methods$default) {
    return(employment.default())
  }
}

# endregion
# exports
# region: exports
box::export(employment, employment.methods)

# endregion
