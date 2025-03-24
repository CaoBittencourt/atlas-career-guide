# setup
# region: imports
box::use(
  assert = mod / utils / assert
)

# endregion
# dispatch
# region: skill set generality
gene <- function(skill_set) {
  # assert args
  assert$models$validate.skill.set(skill_set)

  # generality is the adjusted mean of maxima-normalized attributes
  return(
    (
      1 - sum(
        skill_set / (
          max(skill_set) + (max(skill_set) == 0)
        )
      )
    ) / (
      1 - length(skill_set) * (max(skill_set) != 0)
    )
  )
}

# endregion
# exports
# region: exports
box::export(gene)

# endregion
