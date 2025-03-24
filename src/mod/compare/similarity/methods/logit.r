# setup
# region: imports
box::use(
  fastglm[fastglmPure],
  stats[binomial, coef],
  mod / utils / prob[...]
)

# endregion
# dispatch
# region: logit / probit matching method
similarity.logit <- function(bernoulliAk, bernoulliA, repsÄ, link = c("logit", "probit")[[1]]) {
  # assert args in main function
  # convert to bernoulli in main function
  # run weighted logit or probit regression
  # convert log or prob unit to probability
  return(
    mapply(
      function(ak, aq, äq) {
        aq |>
          as.matrix() |>
          fastglmPure(
            y = ak,
            family = binomial(link),
            weights = äq
          ) |>
          coef() |>
          prob.from(link)
      },
      ak = bernoulliAk,
      aq = bernoulliA,
      äq = repsÄ
    )
  )
}

# endregion
# exports
# region: exports
box::export(similarity.logit)

# endregion
