# setup
# region: imports
box::use(
  labor / employability / misc / ta[...],
  stats[integrate]
)

# endregion
# dispatch
# region: Omega (aggregate time allocation)
Omega.bin <- function(lmin = 0, lmax = 1, ttc) {
  # # assert args
  # stopifnot("'lmin' <= 'lmax' must be responsibility bounds defined in the unit interval." = all(
  #   lmin[[1]] <= lmax[[1]],
  #   lmax[[1]] <= 1,
  #   lmin[[1]] >= 0
  # ))
  #
  # stopifnot("'ttc' must be a task duration function defined in the unit interval." = is.function(ttc))

  # return aggregate normalized task duration (time allocation of responsibility bound = Omega)
  return(
    integrate(
      function(l) {
        ta(l, ttc)
      },
      lower = lmin,
      upper = lmax
    )$value
  )
}

# endregion
# region: vectorized Omega
Omega <- Vectorize(Omega.bin, vectorize.args = c("lmin", "lmax", "ttc"))

# endregion
# exports
# region: exports
box::export(Omega)

# endregion
