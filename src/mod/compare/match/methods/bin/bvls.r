modular::project.options("atlas")
# region: imports
box::use(
  reg.bvls = bvls,
  stats[coef]
)

# endregion
# region: bvls matching method
bvls <- function(ak, aq, äq = rep(1, length(aq))) {
  # assert args in main function
  sqrt(äq) -> äq

  return(
    as.matrix(aq * äq) |>
      reg.bvls$bvls(
        ak * äq,
        bl = 0,
        bu = 1
      ) |>
      coef()
  )
}

# endregion
# region: exports
box::export(bvls)

# endregion
