# generalized logistic curve
logistic <- function(x, a, k, c, q, m, b, nu) {
  return(a + (k - a) / ((c + q * exp(-b * (x - m)))^(1 / nu)))
}

# sigmoid logistic curve
sigmoid <- function(x) {
  return(logistic(x, a = 0, k = 1, c = 1, q = 1, m = 0, b = 1, nu = 1))
}

# # generic linear-logistic method?
# linear.logistic <- function(x, m) {
#   return(logistic(
#     x = x,
#     m = m,
#     a = 0,
#     k = x,
#     c = 1,
#     q = m * (1 - x),
#     nu = x / (m * (x != 1)),
#     # nu = x / m,
#     b = 1 / (1 - m)
#   ))
# }

# linear.logistic <- function(x,m) {
#   return(logistic(
#     x = x/m,
#     m = m/m,
#     a = 0,
#     k = x/m,
#     c = 1,
#     q = 1 * (1 - x/m),
#     nu = (x/m) / ((m/m) * (x/m != 1)),
#     # nu = x/m / 1,
#     b = 1 / (1 - 1)
#   ))
# }
