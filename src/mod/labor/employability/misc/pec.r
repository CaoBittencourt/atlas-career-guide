# setup
# region: imports
box::use(
  cubature[cubintegrate],
  stats[uniroot],
  mod / labor / employability / misc / ta[...]
)

# endregion
# dispatch
# region: proportional employment condition (pec)
# given the pec and a vector of employment levels w, determine optimal responsibility bounds l
pec.l <- function(lmin = 0, wtilde = 1, ttc) {
  # # assert args
  # stopifnot(
  #   "'lmin' must be a lower responsability bound in the unit interval" =
  #     all(
  #       is.numeric(lmin)
  #       , lmin >= 0
  #       , lmin < 1
  #     )
  # )
  #
  # stopifnot(
  #   "'wtilde' must be a relative employment level in the unit interval" =
  #     all(
  #       is.numeric(wtilde)
  #       , wtilde >= 0
  #       , wtilde <= 1
  #     )
  # )
  #
  # stopifnot("'ttc' must be a task duration function defined in the unit interval." = is.function(ttc))

  # solve integral equation for lmax
  uniroot(
    function(lmax) {
      cubintegrate(
        function(l) {
          ta(l, ttc)
        },
        lower = lmin,
        upper = lmax
      )$integral - wtilde[[1]]
    },
    interval = c(0, 1)
    # , interval = c(-.5,1.5)
    , extendInt = "yes"
  )$root -> lmax

  pmax(lmax, 0) -> lmax
  pmin(lmax, 1) -> lmax

  # return optimal responsibility upper bound
  return(lmax)
}

# endregion
# region: vectorized pec
# vectorized pec to find all optimal responsibility bounds
pec <- function(wtilde, ttc) {
  # stopifnot(
  #   "'wtilde must be a numeric vector between 0 and 1, which sums to 1.'" = all(
  #     is.numeric(wtilde),
  #     # sum(wtilde) == 1,
  #     wtilde >= 0,
  #     wtilde <= 1
  #   )
  # )

  lmin <- 0
  v <- 1

  for (wtilde_v in wtilde) {
    c(lmin, pec.l(lmin[[v]], wtilde_v, ttc)) -> lmin

    v <- v + 1
  }

  lmin <- lmin[-1]
  lmin[length(lmin)] <- 1

  return(lmin)
}

# endregion
# exports
# region: exports
box::export(pec)

# endregion
