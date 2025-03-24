# setup
# region: imports
box::use(
  assert = mod / utils / assert,
)

# endregion
# dispatch
# region: dominance
dominance <- function(sqr_unit_mtx, weights = NULL, aggregate = T) {
  # assert args
  assert$base$validate.unit.matrix(sqr_unit_mtx, "sqr_unit_mtx", F)
  assert$base$validate.square(sqr_unit_mtx, "sqr_unit_mtx", F)
  assert$base$validate.bool(aggregate, "aggregate", F)
  assert$base$validate.numeric.bounded(weights, "weights", T, 0)

  # stopifnot(
  #   "'weights' must either be NULL or a numeric vector the same length as the number of rows and columns in 'sqr_unit_mtx'." = any(
  #     !length(weights),
  #     all(
  #       weights |> is.numeric(),
  #       length(weights) == nrow(sqr_unit_mtx)
  #     )
  #   )
  # )

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
# exports
# region: exports
box::export(dominance)

# endregion
