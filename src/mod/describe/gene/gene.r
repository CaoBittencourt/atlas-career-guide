# region: imports
box::use(
  assert = mod / utils / assert
)

# endregion
# region: skill set generality
gene <- function(skill_set) {
  # assert args
  assert$valid_skill_set(skill_set)

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
# region: exports
box::export(gene)

# endregion
