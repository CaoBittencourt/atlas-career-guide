modular::project.options("atlas")
# region: imports
box::use(
  mod / labor / employability / ta / Omega[...]
)

# endregion
# region: employability
employability <- function(hk, Tk, ttc, w, p = w, agg = T) {
  if (is.function(ttc)) {
    list(ttc) -> ttc
  }

  # employability in infinitely stratified labor market
  if (p == Inf) {
    hk * Omega(0, Tk, ttc) -> employability
  }

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
# region: exports
box::export()

# endregion
