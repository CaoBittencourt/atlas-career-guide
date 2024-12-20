# region: imports

# endregion
# region: dominance
dominance <- function(sqr_unit_mtx, weights = NULL, aggregate = T) {
  # assert args
  stopifnot(
    "'sqr_unit_mtx' must be a square numeric matrix in the unit interval." = all(
      any(
        sqr_unit_mtx |> is.matrix(),
        sqr_unit_mtx |> is.data.frame()
      ),
      nrow(sqr_unit_mtx) == ncol(sqr_unit_mtx),
      sqr_unit_mtx |> sapply(is.numeric) |> all(),
      all(sqr_unit_mtx >= 0),
      all(sqr_unit_mtx <= 1)
    )
  )

  stopifnot(
    "'weights' must either be NULL or a numeric vector the same length as the number of rows and columns in 'sqr_unit_mtx'." = any(
      !length(weights),
      all(
        weights |> is.numeric(),
        length(weights) == nrow(sqr_unit_mtx)
      )
    )
  )

  stopifnot(
    "'aggregate' must be either TRUE or FALSE." = all(
      is.logical(aggregate),
      !is.na(aggregate)
    )
  )

  # default weights
  if (!length(weights)) {
    weights <- rep(1, ncol(sqr_unit_mtx))
  }

  # dominance matrix
  sqr_unit_mtx - t(sqr_unit_mtx) -> dominance

  # aggregate dominance vector
  if (aggregate) {
    w <- sum(weights)
    ((w - weights) + colSums(dominance)) / (2 * (w - weights)) -> dominance
  }

  # output dominance coefficients
  return(dominance)
}

# endregion
# region: exports
box::export(dominance)

# endregion
