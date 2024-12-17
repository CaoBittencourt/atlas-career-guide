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
# region: [VECTORIZE?] skill set versatility
versatility <- function(skill_set, macroflex) {
  # assert args
  assert$valid_skill_set(skill_set)

  stopifnot(
    "'macroflex' must be a numeric vector in the unit interval the same length as 'skill_set'." = all(
      is.numeric(macroflex),
      macroflex >= 0,
      macroflex <= 1,
      length(macroflex) == length(skill_set)
    )
  )

  # a skill set's versatility is their aggregate human capital macroflexibility
  # do not employ attribute equivalence in weighting: weights need to be linear
  return(
    weighted.mean(
      x = macroflex,
      w = skill_set
    )
  )
}

# df_skill_mtx |> t() |> as.data.frame() |> sapply(versatility, macroflex = capital_macroflex)

# endregion
# region: exports
box::export(Phi, versatility)

# endregion
