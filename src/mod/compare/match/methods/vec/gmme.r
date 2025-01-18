# note: this equivalent to the cobb-douglas function
gmme <- function(skill_set, skill_mtx, weights = NULL, zeros = 1 / 100) {
  # assert args in main function
  # estimate similarity as geometric mean of relative attribute scores
  (skill_set |> pmax(zeros) / skill_mtx) |> pmin(1) -> mtx_prod
  (mtx_prod^weights) |> apply(2, prod) -> mtx_prod
  mtx_prod^(1 / sum(weights)) -> mtx_prod
  return(mtx_prod)
}
