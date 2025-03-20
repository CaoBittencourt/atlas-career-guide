# setup
# region: imports
box::use(
  assert = mod / utils / assert,
)

# endregion
# methods
# region: numeric method
as.bernoulli.numeric <- function(x, ub, lb, scaling) {
  # assert args in main function

  # apply scaling factor
  if (length(scaling)) {
    x * scaling[[1]] -> x
  }

  # repeats 1 for every "point" of ak above lb
  # repeats 0 for every "point" of ak below ub
  return(
    c(
      as.integer(x) |>
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
    # c(
    #   as.integer(ub[[1]] * x) |>
    #     vapply(
    #       function(a) {
    #         rep(
    #           c(1L, 0L),
    #           times = ceiling(
    #             c(a, (ub[[1]] - lb[[1]]) - a)
    #           )
    #         )
    #       },
    #       numeric(ub[[1]])
    #     )
    # )
  )
}


# endregion
# region: matrix method
as.bernoulli.matrix <- function(x, ub, lb, scaling) {
  # assert args in main function

  # apply scaling factor
  if (length(scaling)) {
    x * scaling[[1]] -> x
  }

  # apply bernoulli function
  return(
    x |> vapply(
      as.bernoulli.numeric,
      numeric(nrow(x) * ub[[1]]),
      ub = ub[[1]],
      lb = lb[[1]],
      scaling = scaling[[1]]
    )
  )
}

# endregion
# dispatch
# region: bernoulli generic function
as.bernoulli <- function(x, ub, lb, scaling = NULL) {
  # assert args
  assert$base$validate.numeric(x, "x", F)
  assert$base$validate.numeric(ub, "ub", F)
  assert$base$validate.numeric(lb, "lb", F)
  assert$base$validate.numeric(scaling, "scaling", T)

  stopifnot(
    "'x' must be between 'ub' and 'lb'." =
      all(x >= lb[[1]], x <= ub[[1]])
  )

  stopifnot(
    "'ub' must be greater than 'lb'." =
      c(lb[[1]] < ub[[1]])
  )

  # multiple dispatch
  if (!assert$base$is.matrix.like(x)) {
    return(as.bernoulli.numeric(x, ub, lb, scaling))
  }

  return(as.bernoulli.matrix(x, ub, lb, scaling))
}

# endregion
# exports
# region: exports
box::export(as.bernoulli)

# endregion
