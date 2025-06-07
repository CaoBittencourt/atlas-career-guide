# setup
# region: imports
box::use(
  assert = mod / utils / assert,
  mod / roadmap / path / functions / prob[...],
)

# endregion
# methods
# region: default method
payoff.default <- function(skq, prob = 1) {
  # assert args in main function
  # expected payoff
  # - E[U] = Pr[v2] * u(v2) = ((w(v2) / w) * s(v1, v2) * [s(v1, v2) >= 0.5]) * u(v2) >= 0
  #         - cost(v1,v2)
  #         - weight := cost * ((1 - E[u]) ^ !is.infinity(cost))
  return("default")
}

# endregion
# region: list of methods
list(
  "default" = "default"
) -> payoff.methods

# endregion
# dispatch
# region: payoff generic function
payoff <- function(skq, ukq = 1, w = NULL, payoff_method = payoff.methods[[1]], ...) {
  # assert args

  # multiple dispatch
  if (payoff_method[[1]] == payoff.methods$default) {
    return(payoff.default())
  }
}

# endregion
# exports
# region: exports
box::export(payoff, payoff.methods)

# endregion
