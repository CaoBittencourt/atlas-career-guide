# region: imports
box::use(
  mod / utils / bernoulli[...],
  fastglm[fastglmPure],
  stats[binomial, coef],
  mod / utils / prob[...]
)

# endregion
# region: logit / probit matching method
logit <- function(ak, aq, äq = rep(1, length(aq)), link = c("logit", "probit")[[1]]) {
  # assert args in main function

  # convert to bernoulli
  ak |> as.bernoulli(ub = 100, lb = 0) -> ak
  aq |> as.bernoulli(ub = 100, lb = 0) -> aq
  äq |> rep(each = 100 - 0) -> äq

  # run weighted logit or probit regression
  # convert log or prob unit to probability
  return(
    aq |>
      as.matrix() |>
      fastglmPure(
        y = ak,
        family = binomial(link),
        weights = äq
      ) |>
      coef() |>
      prob.from(link)
  )
}

# endregion
# region: exports
box::export(logit)

# endregion
