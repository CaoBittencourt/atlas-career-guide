gmme <- function(ak, aq, äq = rep(1, length(aq)), zeros = 1 / 100) {
  # assert args in main function
  # estimate similarity as weighted geometric mean of relative attributes
  return(prod(((ak |> pmax(zeros) / aq) |> pmin(1))^äq)^(1 / sum(äq)))
}
