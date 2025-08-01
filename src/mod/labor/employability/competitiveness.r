# setup
# region: imports
box::use(
  labor / employability / misc / Omega[...],
  stats[weighted.mean]
)

# endregion
# dispatch
# region: competitiveness
competitiveness <- function(T_q, h_q, u_qk, u_qq, ttc, w, agg = T) {
  if (is.function(ttc)) {
    list(ttc) -> ttc
  }

  # competitiveness in infinitely stratified labor market
  (u_qk >= u_qq) * h_q * Omega(0, T_q, ttc) -> vstilde

  # aggregate competitiveness
  if (agg) {
    weighted.mean(
      x = vstilde,
      w = w
    ) -> vstilde
  }

  # competitiveness
  return(vstilde)
}

# endregion
# exports
# region: exports
box::export(competitiveness)

# endregion
