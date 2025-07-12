# region: imports

# endregion
# region: numeric method
# convert bounded variable to bernoulli vector
as.bernoulli.num <- function(x, ub = 100, lb = 0) {
  # assert args in main function
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

# endregion
# region: data frame / matrix method
as.bernoulli.mtx <- function(x, ub = 100, lb = 0) {
  # assert args in main function
  return(x |> vapply(as.bernoulli.num, numeric(nrow(x) * ub[[1]]), ub = ub[[1]], lb = lb[[1]]))
}

# endregion
# region: generic function
as.bernoulli <- function(x, ub = 100, lb = 0) {
  # assert args
  stopifnot(
    "'x' must be a numeric vector." =
      x |> sapply(is.numeric) |> all()
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

  # multiple dispatch
  if (!length(dim(x))) {
    return(as.bernoulli.num(x))
  }

  return(as.bernoulli.mtx(x))
}

# endregion
# region: exports
box::export(as.bernoulli)

# endregion
