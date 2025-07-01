# region: imports
box::use(
  cubature[cubintegrate],
  stats[uniroot],
  labor / employability / misc / ta[...]
)

# endregion
# region: proportional employment condition (pec)
# given the pec and a vector of employment levels w, determine optimal responsibility bounds l
pec.l <- function(lmin = 0, wtilde = 1, ttc) {
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
    interval = c(0, 1),
    extendInt = "yes"
  )$root -> lmax

  pmax(lmax, 0) -> lmax
  pmin(lmax, 1) -> lmax

  # return optimal responsibility upper bound
  return(lmax)
}

# endregion
# region: recursive pec
# vectorized pec to find all optimal responsibility bounds
pec <- function(w, ttc) {
  options(expressions = 500000)

  pec.rec <- function(lmin, wtilde, w, ttc) {
    if (all(w > 0, lmin < 1)) {
      print(lmin)
      return(
        c(
          lmin,
          pec.rec(
            pec.l(
              lmin[length(lmin)],
              wtilde,
              ttc
            ),
            wtilde,
            w - 1,
            ttc
          )
        )
      )
    } else {
      return(1)
    }
  }

  pec.rec(0, 1 / w, w, ttc) -> l
  l[length(l)] <- 1

  return(l)
}

# endregion
# region: exports
box::export(pec)

# endregion
