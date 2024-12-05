cobb_douglas <- function(skill_set, skill_mtx, weights = NULL) {
  # assert args in main function
  # skill_set: a skill set in the unit interval to compare with skill_mtx
  # skill_mtx: a skill set matrix in the unit interval to compare with skill_set
  # weights: a vector of attribute weights for each skill set in skill_mtx

  # truncate skill set by the skill set matrix
  # this allows for moderate attribute substitution
  # but disallows attribute "over-substitution"
  skill_mtx |> pmin(skill_set) -> ss

  # normalize weights by sum
  (weights / colSums(weights)) -> weights

  # assess similarity as normalized weighted product
  return(
    apply(ss^weights, 2, prod) /
      apply(skill_mtx^weights, 2, prod)
  )
}

# runif(120) -> ss

# replicate(
#   n = 873,
#   runif(120)
# ) -> mtx_ss

# library(atlas.aeq)
# mtx_ss |> apply(2, fun_aeq_aequivalence) -> mtx_aeq

# mtx_ss |> dim()
# mtx_aeq |> dim()
# (mtx_ss^mtx_aeq) |> dim()
# ss |> length()

# cobb_douglas(ss, mtx_ss, mtx_aeq)
