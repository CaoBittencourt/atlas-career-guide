# region: imports
box::use(assert = mod / utils / assert)

# endregion
# # region: skill set generality
# gene <- function(skill_set) {
#   # assert args
#   assert$valid_skill_set(skill_set)

#   # generality is the mean of maxima-normalized attributes
#   return(mean(skill_set / max(skill_set)))
# }

# # endregion
# region: skill set generality
gene <- function(skill_set) {
  # assert args
  assert$valid_skill_set(skill_set)

  # generality is the adjusted mean of maxima-normalized attributes
  return(
    (
      1 - sum(skill_set / max(skill_set))
    ) / (
      1 - length(skill_set)
    )
  )
}

# endregion
# region: exports
box::export(gene)

# endregion
