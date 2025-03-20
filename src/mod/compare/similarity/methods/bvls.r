# region: imports
box::use(
  reg.bvls = bvls,
  stats[coef]
)

# endregion
# region: bvls matching method
similarity.bvls <- function(Ak, A, sqrtÄ) {
  # assert args in main function
  # bounded variable least squares
  mapply(
    function(ak, aq) {
      aq |>
        as.matrix() |>
        reg.bvls$bvls(
          ak,
          bl = 0,
          bu = 1
        ) |>
        coef()
    },
    ak = Ak * sqrtÄ,
    aq = A * sqrtÄ
  )
}

# endregion
# region: exports
box::export(similarity.bvls)

# endregion
