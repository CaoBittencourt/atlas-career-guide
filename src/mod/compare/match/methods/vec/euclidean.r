euclidean <- function(Ak, A, Ä) {
  # assess similarity from weighted euclidean distance normalized by maximum distance
  return(1 - sqrt(colSums(Ä * (Ak - A)^2)) / sqrt(colSums(Ä * pmax(1 - A, A - 0)^2)))
}
