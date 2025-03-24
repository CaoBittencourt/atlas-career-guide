# setup
# region: imports
box::use(
  stats[integrate]
)

# endregion
# dispatch
# region: time allocation
# time allocation (ta) = normalized task duration (ttc)
ta <- function(l, ttc) {
  # return normalized task duration
  return(ttc(l) / integrate(ttc, 0, 1)$value)
}

# endregion
# exports
# region: exports
box::export(ta)

# endregion
