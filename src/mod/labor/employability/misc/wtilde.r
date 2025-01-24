# region: imports
box::use(
  mod / labor / employability / misc / pec[...]
)

# endregion
# region: relative employment levels in maximum labor stratification
wtilde.mls <- function(w) {
  round(w) |> pmax(1) -> w
  return(rep(1 / w, w))
}

# endregion
# region: kernel density approaximation of optimal responsibility bounds
l.kde <- function(wq.proxy = 1024, wq, ttc) {
  # assert args
  stopifnot(
    "'wq' must be a non-negative integer indicating the workforce size." = is.numeric(wq)
  )

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
# region: exports
box::export(l.kde)

# endregion
