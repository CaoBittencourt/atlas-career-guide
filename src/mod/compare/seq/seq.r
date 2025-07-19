# setup
# region: imports
box::use(
  assert = utils / assert,
  glogis = utils / logistic
)

# endregion
# methods
# region: linear_logistic method
ss.linear_logistic <- function(s) {
  # assert args in main function
  # apply generalized logistic function with parameters
  return(
    glogis$logistic(
      x = s,
      m = 0.5,
      a = 0,
      k = s,
      c = 1,
      q = 0.5 * (1 - s),
      nu = s / (0.5 * (s != 1)),
      b = 1 / (1 - 0.5)
    )
  )
}

# endregion
# region: relu method
ss.relu <- function(s) {
  # assert args in main function
  # relu activation function
  return(pmax(s, 0.5))
}

# endregion
# region: list of methods
list(
  'linear_logistic' = 'linear-logistic',
  'relu' = 'relu'
) -> ss.methods

# endregion
# dispatch
# region: ss generic function
ss <- function(similarity, ss_method = ss.methods[[1]], ...) {
  # assert args
  assert$base$validate.numeric.bounded(similarity, "similarity", F, 0, 1)

  # multiple dispatch
  if (ss_method[[1]] == ss.methods$linear_logistic) {
    return(ss.linear_logistic(similarity))
  }

  if (ss_method[[1]] == ss.methods$relu) {
    return(ss.relu(similarity))
  }
}

# endregion
# exports
# region: exports
box::export(ss, ss.methods)

# endregion
