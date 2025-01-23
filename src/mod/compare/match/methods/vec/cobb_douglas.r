cobb_douglas <- function(Ak, A, Ä = NULL) {
  # assert args in main function

  # zeros = 0 => not having even one unimportant skill nullifies production
  # zeros = 1 => not having even an important skill doesn't affect production
  # zeros = 0.5 => skills are all equally important in determining production
  # zeros = 1 - A => not having a skill does not necessarily nullify production,
  # however the more important the skill, the higher the impact of not having it
  # Ak + (Ak == 0) * (1 - A) -> Ak
  # Ak + (Ak == 0) * (1 / 100) -> Ak
  # Ak + (Ak == 0) * (1 - Ä) -> Ak
  Ak + (Ak == 0) * A * (1 - Ä) -> Ak

  # truncate skill set by the skill set matrix
  # this allows for moderate attribute substitution
  # but disallows attribute "over-substitution"
  (Ak > A) * A + (Ak <= A) * Ak -> Ak

  # normalize weights by sum
  Ä / colSums(Ä) -> Ä

  # assess similarity as normalized weighted product
  return(vapply(Ak^Ä, prod, numeric(1)) / vapply(A^Ä, prod, numeric(1)))
}

# cobb_douglas <- function(skill_set, skill_mtx, weights = NULL, zeros = 1 - skill_mtx) {
#   # assert args in main function
#   # truncate skill set by the skill set matrix
#   # this allows for moderate attribute substitution
#   # but disallows attribute "over-substitution"
#   skill_mtx |> pmin(skill_set |> pmax(zeros)) -> ss

#   # normalize weights by sum
#   (weights / colSums(weights)) -> weights

#   # assess similarity as normalized weighted product
#   return(
#     apply(ss^weights, 2, prod) /
#       apply(skill_mtx^weights, 2, prod)
#   )
# }
