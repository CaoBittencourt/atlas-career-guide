# region: imports
box::use(
  stats[pnorm]
)

# endregion
# region: probability from logit and probit
prob.from <- function(z, link = c("logit", "probit")[[1]]) {
  # assert args
  stopifnot(
    "'z' must be numeric." = is.numeric(z)
  )

  stopifnot(
    "'link' must be either 'logit' or 'probit'" = any(
      link[[1]] == c("logit", "probit")
    )
  )

  # inverse log-odds
  if (link == "logit") {
    return(exp(z) / (1 + exp(z)))
  }

  # inverse cdf of normal distribution
  return(pnorm(z))
}

# endregion
# region: exports
box::export(prob.from)

# endregion
