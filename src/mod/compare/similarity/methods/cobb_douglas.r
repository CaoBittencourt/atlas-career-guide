similarity.cobb_douglas <- function(Ak, A, Ä = NULL) {
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
