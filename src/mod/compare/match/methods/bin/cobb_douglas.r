cobb_douglas <- function(ak, aq, äq = rep(1, length(aq)), zeros = 1 / 100) {
  # cobb_douglas <- function(ak, aq, äq = rep(1, length(aq)), zeros = 1 - aq) {
  # assert args in main function

  # truncate skill set by the skill set matrix
  # this allows for moderate attribute substitution
  # but disallows attribute "over-substitution"
  aq |> pmin(ak |> pmax(zeros)) -> ak

  # normalize weights by sum
  äq / sum(äq) -> äq

  # assess similarity as normalized weighted product
  return(prod(ak^äq) / prod(aq^äq))
}
