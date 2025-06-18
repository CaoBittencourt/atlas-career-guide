# setup
# region: imports
box::use(
  assert = mod / utils / assert,
  mod / roadmap / path / functions / prob[...],
)

# endregion
# methods
# region: default method
inverse.payoff.default <- function(payoff) {
  # assert args in main function
  return(1 / log(1 + payoff))
}

# endregion
# region: list of methods
list(
  "default" = "default"
) -> inverse.payoff.methods

# endregion
# dispatch
# region: inverse payoff generic function
inverse.payoff <- function(payoff, inverse.payoff_method = inverse.payoff.methods[[1]], ...) {
  # assert args

  # multiple dispatch
  if (inverse.payoff_method[[1]] == inverse.payoff.methods$default) {
    return(inverse.payoff.default(payoff))
  }
}

# endregion
# exports
# region: exports
box::export(inverse.payoff, inverse.payoff.methods)

# endregion
