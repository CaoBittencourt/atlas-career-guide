# region: imports
box::use(
  assert = mod / utils / assert,
  stats[weighted.mean]
)

# endregion
# region: human capital macroflexibility (uppercase phi)
Phi <- function(skill_mtx, weights = NULL) {
  # assert args
  assert$valid_skill_mtx(skill_mtx)

  stopifnot(
    "'weights' must be either NULL or a non-negative numeric vector the same length as the number of rows in 'skill_mtx'." = any(
      all(
        is.numeric(weights),
        length(weights) == nrow(skill_mtx),
        weights >= 0
      ),
      is.null(weights)
    )
  )

  if (!is.data.frame(skill_mtx)) {
    skill_mtx |> as.data.frame() -> skill_mtx
  }

  if (is.null(weights)) {
    rep(1, nrow(skill_mtx)) -> weights
  }

  # macroflexibility = attributes' weighted penalized mlv (i.e. mean)
  return(skill_mtx |> sapply(weighted.mean, w = weights))
}

# endregion
# region: exports
box::export(Phi)

# endregion
