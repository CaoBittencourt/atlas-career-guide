# setup
# region: imports
box::use(
  assert = mod / utils / assert,
)

# endregion
# methods
# region: default method
cost_util.default <- function(cost, util) {
  # assert args in main function
  # simple division
  return(cost / util)
}

# endregion
# region: payoff method
cost_util.payoff <- function(cost, util) {
  # assert args in main function
  # normalized payoff
  return(cost / util)
}

# endregion
# region: list of methods
list(
  "default" = "default"
) -> cost_util.methods

# endregion
# dispatch
# region: cost-utility efficiency generic function
cost_util <- function(cost, util, cost_util_method = cost_util.methods[[1]], ...) {
  # assert args in main function
  assert$base$validate.method(cost_util_method, "cost_util_method", cost_util.methods)

  # multiple dispatch
  if (cost_util_method[[1]] == cost_util.methods$default) {
    return(cost_util.default(cost, util))
  }
}

# endregion
# exports
# region: exports
box::export(cost_util, cost_util.methods)

# endregion
