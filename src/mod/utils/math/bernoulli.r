# convert bounded variable to bernoulli vector
as.bernoulli <- function(x, ub = 100, lb = 0) {
  # assert args
  stopifnot(
    "'x' must be a numeric vector." = is.numeric(x)
  )

  stopifnot(
    "'ub' must be numeric." = is.numeric(ub)
  )

  stopifnot(
    "'lb' must be numeric." = is.numeric(lb)
  )

  stopifnot(
    "'x' must be between 'ub' and 'lb'." =
      all(x >= lb[[1]], x <= ub[[1]])
  )

  stopifnot(
    "'ub' must be greater than 'lb'." =
      c(lb[[1]] < ub[[1]])
  )

  # repeats 1 for every "point" of ak above lb
  # repeats 0 for every "point" of ak below ub
  return(
    c(
      as.integer(ub[[1]] * x) |>
        vapply(
          function(a) {
            rep(
              c(1L, 0L),
              times = ceiling(
                c(a, (ub[[1]] - lb[[1]]) - a)
              )
            )
          },
          numeric(ub[[1]])
        )
    )
  )
}
