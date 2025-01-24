# employment level proportion in maximum labor stratification
wtilde.mls <- function(w) {
  round(w) |> pmax(1) -> w
  return(rep(1 / w, w))
}

l.kde <- function(wq = 1024, ttc, reps = 100) {
  # assert args
  stopifnot(
    "'wq' must be a non-negative integer indicating the workforce size." = is.numeric(wq)
  )

  wq |>
    ceiling() |>
    pmax(1) ->
  wq

  wq |> wtilde.mls(reps) -> wtilde

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
