# setup
# region: imports
box::use(
  assert = utils / assert,
  stats[weighted.mean, setNames]
)

# endregion
# dispatch
# region: macroflex generic function
macroflex <- function(skill_mtx, employment = NULL, skill.names = NULL) {
  # assert args
  assert$models$as.skill.set.matrix(skill_mtx, "skill_mtx") -> skill_mtx
  assert$models$validate.employment(employment, "employment", T)

  stopifnot(
    "'employment' must be either NULL or a non-negative numeric vector the same length as the number of rows in 'skill_mtx'." = any(
      all(
        is.numeric(employment),
        length(employment) == ncol(skill_mtx),
        employment >= 0
      ),
      is.null(employment)
    )
  )

  if (!length(employment)) {
    rep(1, ncol(skill_mtx)) -> employment
  }

  # macroflexibility = attributes' weighted penalized mlv (i.e. mean)
  return(skill_mtx |> apply(1, weighted.mean, w = employment) |> setNames(skill.names))
}

# endregion
# exports
# region: exports
box::export(macroflex)

# endregion
