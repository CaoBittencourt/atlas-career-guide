# setup
# region: imports
box::use(
  assert = utils / assert,
  labor / employability / misc / Omega[...],
  # labor / employability / misc / pec[...],
  # labor / employability / misc / Tkappa[...],
  stats[weighted.mean]
)

# endregion
# dispatch
# region: employability
employability <- function(Tk, hk, ttc, w, agg = T) {
  # assert args
  assert$base$validate.numeric.bounded(hk, "hk", F, 0, 1)
  assert$base$validate.numeric.bounded(Tk, "Tk", F, 0, 1)
  assert$base$validate.numeric.bounded(w, "w", F, 0, lc = F)
  assert$base$validate.bool(agg, "agg", F)

  stopifnot(
    "'ttc' must be a function or a list of functions." = any(
      is.function(ttc),
      all(
        is.list(ttc),
        ttc |> sapply(is.function) |> all()
      )
    )
  )

  if (is.function(ttc)) {
    list(ttc) -> ttc
  }

  # employability in infinitely stratified labor market
  # employability in maximally stratified labor markets
  hk * Omega(0, Tk, ttc) -> employability

  # aggregate employability
  if (agg) {
    weighted.mean(
      x = employability,
      w = w
    ) -> employability
  }

  # return employability
  return(employability)
}

# endregion
# exports
# region: exports
box::export(employability)

# endregion
