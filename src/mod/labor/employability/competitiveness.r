# region: imports
box::use(
  mod / labor / employability / misc / Omega[...],
  stats[weighted.mean]
)

# endregion
# region: competitiveness
competitiveness <- function(h_q, T_q, u_qk, u_qq, ttc, w, agg = T) {
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
# region: exports
box::export(competitiveness)

# endregion
