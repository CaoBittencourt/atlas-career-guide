# setup
# region: imports
box::use(
  labor / employability / misc / pec[...],
  assert = utils / assert
)

# endregion
# dispatch
# region: relative employment levels in maximum labor stratification
wtilde.mls <- function(w) {
  round(w) |> pmax(1) -> w
  return(rep(1 / w, w))
}

# endregion
# region: kernel density approximation of optimal responsibility bounds
l.kde <- function(wq.proxy = 1024, wq, ttc) {
  # assert args
  assert$base$validate.numeric.bounded(wq, "wq", F, 0, lc = F)

  wq |>
    ceiling() |>
    pmax(1) ->
  wq

  wq.proxy |> wtilde.mls() -> wtilde

  # return kernel density estimation of productivity requirements
  return(
    (
      wtilde |>
        pec(ttc) |>
        density(
          weights = wtilde,
          from = 0,
          to = 1,
          n = wq
        )
    )$x
  )
}

# endregion
# exports
# region: exports
box::export(l.kde)

# endregion
