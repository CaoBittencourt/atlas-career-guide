# region: imports
box::use(
  assert = mod / utils / assert,
  stats[weighted.mean, setNames]
)

# endregion
# region: human capital macroflexibility (uppercase phi)
macroflex <- function(skill_mtx, weights = NULL, skill.names = NULL) {
  # assert args
  assert$as.skill_mtx(skill_mtx) -> skill_mtx

  stopifnot(
    "'weights' must be either NULL or a non-negative numeric vector the same length as the number of rows in 'skill_mtx'." = any(
      all(
        is.numeric(weights),
        length(weights) == ncol(skill_mtx),
        weights >= 0
      ),
      is.null(weights)
    )
  )

  if (!is.data.frame(skill_mtx)) {
    skill_mtx |> as.data.frame() -> skill_mtx
  }

  if (is.null(weights)) {
    rep(1, ncol(skill_mtx)) -> weights
  }

  # macroflexibility = attributes' weighted penalized mlv (i.e. mean)
  return(skill_mtx |> apply(1, weighted.mean, w = weights) |> setNames(skill.names))
}

# endregion
# region: skill set versatility
versatility <- function(skill_set, macroflex) {
  # assert args
  assert$as.skill_mtx(skill_set) -> skill_set

  stopifnot(
    "'macroflex' must be a numeric vector in the unit interval the same length as 'skill_set'." = all(
      is.numeric(macroflex),
      macroflex >= 0,
      macroflex <= 1,
      length(macroflex) == nrow(skill_set)
    )
  )

  # a skill set's versatility is their aggregate human capital macroflexibility
  # do not employ attribute equivalence in weighting: weights need to be linear
  return(
    skill_set |>
      vapply(
        function(w) {
          weighted.mean(
            x = macroflex,
            w = w
          )
        },
        numeric(1)
      )
  )
}

# endregion
# region: exports
box::export(macroflex, versatility)

# endregion
