# setup
# region: imports
box::use(
  assert = mod / utils / assert,
  stats[weighted.mean, setNames]
)

# endregion
# dispatch
# region: versatility generic function
versatility <- function(skill_set, macroflex) {
  # assert args
  assert$models$as.skill.set.matrix(skill_set, "skill_set") -> skill_set
  assert$models$validate.macroflex(macroflex, "macroflex")

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
# exports
# region: exports
box::export(versatility)

# endregion
