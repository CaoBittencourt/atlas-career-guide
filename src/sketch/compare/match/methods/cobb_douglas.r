cobb_douglas <- function(skill_set, skill_mtx, weights = NULL, zeros = 1 / 100) {
  # assert args in main function
  # skill_set: a skill set in the unit interval to compare with skill_mtx
  # skill_mtx: a skill set matrix in the unit interval to compare with skill_set
  # weights: a vector of attribute weights for each skill set in skill_mtx
  # zeros: a number to substitute zeros so that the whole expression is not null when an attribute is zero

  # truncate skill set by the skill set matrix
  # this allows for moderate attribute substitution
  # but disallows attribute "over-substitution"
  skill_mtx |> pmin(skill_set |> pmax(zeros)) -> ss

  # normalize weights by sum
  (weights / colSums(weights)) -> weights

  # assess similarity as normalized weighted product
  return(
    apply(ss^weights, 2, prod) /
      apply(skill_mtx^weights, 2, prod)
  )
}
