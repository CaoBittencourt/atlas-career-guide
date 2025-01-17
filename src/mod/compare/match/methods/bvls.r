# region: imports
box::use(
  bvls[bvls],
  stats[coef]
)

# endregion
# region: bvls matching method
s.bvls <- function(ak, aq, äq = rep(1, length(aq))) {
  # assert args in main function
  sqrt(äq) -> äq

  return(
    as.matrix(aq * äq) |>
      bvls(
        ak * äq,
        bl = 0,
        bu = 1
      ) |>
      coef()
  )
}

# endregion
# region: exports
box::export(s.bvls)

# endregion
