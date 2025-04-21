# setup
# region: imports
box::use(
  assert = mod / utils / assert,
)

# endregion
# methods
# region: method1 method
cost_util.method1 <- function(cost, util) {
  # assert args in main function
  # simple division
  return(cost / util)
}

# endregion
# region: list of methods
list(
  "method1" = "method1"
) -> cost_util.methods

# endregion
# dispatch
# region: cost-utility efficiency generic function
cost_util <- function(cost, util, cost_util_method = cost_util.methods[[1]], ...) {
  # assert args in main function
  assert$base$validate.method(cost_util_method, "cost_util_method", cost_util.methods)

  # multiple dispatch
  if (cost_util_method[[1]] == cost_util.methods$method1) {
    return(cost_util.method1(cost, util))
  }
}

# endregion
# exports
# region: exports
box::export(cost_util, cost_util.methods)

# endregion
