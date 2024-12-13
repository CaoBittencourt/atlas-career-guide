# generalized logistic curve
logistic <- function(x, a, k, c, q, m, b, nu) {
  return(a + (k - a) / ((c + q * exp(-b * (x - m)))^(1 / nu)))
}

# sigmoid logistic curve
sigmoid <- function(x) {
  return(logistic(x, a = 0, k = 1, c = 1, q = 1, m = 0, b = 1, nu = 1))
}
