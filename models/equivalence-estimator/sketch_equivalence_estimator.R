fun_scale <- function(x, m){
  
  ((1 - x) * (1 - m)) ^ - (x - m)
  # 1 / m
  # (x/x) / (1 - m)
  # 1 / (1 + m)
  # 1 / ((1 - x) * (1 - m))
  # 1 / (x * (1 - m))
  # 1 / ((x / x) - m)
  # ((x/x) / (1 - m)) ^ (1 / (1 - x))
  # ((1 / m) - x) / m
  
}

curve(fun_scale(x = x, m = 0), from = 0, to = 1, col = 'red', ylim = c(0,100))
# curve(fun_scale(x = x, m = 0), from = 0, to = 1, col = 'red')
curve(expr = 30 * (1 + x - x), from = 0, to = 1, add = T)


fun_scale(x = seq(0, 1, .1), m = 0) |> round(2) |> setNames(seq(0, 1, .1))
fun_scale(x = seq(0, 1, .1), m = 0.1) |> round(2) |> setNames(seq(0, 1, .1))
fun_scale(x = seq(0, 1, .1), m = 0.25) |> round(2) |> setNames(seq(0, 1, .1))
fun_scale(x = seq(0, 1, .1), m = 0.5) |> round(2) |> setNames(seq(0, 1, .1))
fun_scale(x = seq(0, 1, .1), m = 0.75) |> round(2) |> setNames(seq(0, 1, .1))
fun_scale(x = seq(0, 1, .1), m = 0.9) |> round(2) |> setNames(seq(0, 1, .1))
fun_scale(x = seq(0, 1, .1), m = 1) |> round(2) |> setNames(seq(0, 1, .1))

fun_eqvl <- function(x, m){
  # good models
  x ^ (
    (1 / x) ^
      (m / (1 - m))
  )
  
  # # # working, but not ideal
  # x ^ (m / (1 - m))
  # # default
  # # x ^ (1 / x)
  
  # # # non working
  # x ^ (
  #   (1 / x) ^
  #     (m / (1 - m))
  #   # (1 / (1 - m)) # not linear for m = 0
  # )
  # # linear interpolation
  # (x) * x + 
  #   (1 - x) * x ^ (1 / m)
  # # NaN
  # x ^ ((1 / x) * (m / (1 - m)))
  # x ^ ((1 - m) / x)
  # x ^ (m / x)
  
}

# fun_eqvl(0,1)
# fun_eqvl(0,0)
# 
# fun_eqvl(1,1)
# fun_eqvl(1,0)
# 
# fun_eqvl(0.9,1)
# fun_eqvl(0.5,1)
# fun_eqvl(0.25,1)
# fun_eqvl(0.1,1)
# fun_eqvl(0.99,1)
# fun_eqvl(1,1)
# 
# fun_eqvl(0.9,0.9)
# fun_eqvl(0.5,0.5)
# 
# fun_eqvl(0.7,0.9)
# fun_eqvl(0.3,0.5)
# 
# fun_eqvl(1,0.9)
# fun_eqvl(1,0.5)

fun_eqvl_logistic <- function(
    x
    , m
    , a = 0
    # , b = 100
    # , b = 37
    , b = 37
    # , b = exp(1) * 10
    # , b = exp(m) * 1 / m
    # , b = exp(m) * 10
    # , b = exp(m * ) * 10
    # , b = 1
    # , b = (1 / m) ^ 2
    # , b = 10
    # , b = Inf
    # , b = 1 / m
    # , b = x / m
    # , b = (1 / x) ^ (m / (1 - m))
    # , b = (1 / x * m) ^ (m / (1 - m))
    # , b = (m / x) ^ (m / (1 - m))
    # , b = (1 / m) ^ (1 / (1 - m))
    # , b = (1 / m) ^ (x / (1 - m))
    # , b = (x / m) ^ (1 / (1 - m))
    # , b = (x / m) ^ (1 / x)
    # , b = m / (x * (1 - m))
    # , b = (1 / x) ^ ((1 / x) ^ (m / (1 - m)))
    # , b = (1 / x) ^ ((1 / x) * (m / (1 - m)))
    , c = 1
    , k = x
    , nu = (1 - m) / (1 - x)
    , q = m * (1 - x)
){
  
  # generalized logistic function
  a +
    (k - a) / (
      (c + q * exp(- b * (x - m))) ^ (1 / nu)
    ) -> eqvl
  
  return(eqvl)
  
}

# fun_eqvl(seq(0, 1, .1), m = 0) |> round(2) |> setNames(seq(0, 1, .1))
# fun_eqvl(seq(0, 1, .1), m = 0.25) |> round(2) |> setNames(seq(0, 1, .1))
# fun_eqvl(seq(0, 1, .1), m = 0.5) |> round(2) |> setNames(seq(0, 1, .1))
# fun_eqvl(seq(0, 1, .1), m = 0.75) |> round(2) |> setNames(seq(0, 1, .1))
# fun_eqvl(seq(0, 1, .1), m = 0.9) |> round(2) |> setNames(seq(0, 1, .1))
# fun_eqvl(seq(0, 1, .1), m = 0.95) |> round(2) |> setNames(seq(0, 1, .1))
# fun_eqvl(seq(0, 1, .1), m = 0.99) |> round(2) |> setNames(seq(0, 1, .1))
# fun_eqvl(seq(0, 1, .1), m = 1) |> round(2) |> setNames(seq(0, 1, .1))

fun_eqvl_logistic(seq(0, 1, .1), m = 0) |> round(2) |> setNames(seq(0, 1, .1))
fun_eqvl_logistic(seq(0, 1, .1), m = 0.25) |> round(2) |> setNames(seq(0, 1, .1))
fun_eqvl_logistic(seq(0, 1, .1), m = 0.5) |> round(2) |> setNames(seq(0, 1, .1))
fun_eqvl_logistic(seq(0, 1, .1), m = 0.75) |> round(2) |> setNames(seq(0, 1, .1))
fun_eqvl_logistic(seq(0, 1, .1), m = 0.9) |> round(2) |> setNames(seq(0, 1, .1))
fun_eqvl_logistic(seq(0, 1, .1), m = 0.95) |> round(2) |> setNames(seq(0, 1, .1))
fun_eqvl_logistic(seq(0, 1, .1), m = 0.99) |> round(2) |> setNames(seq(0, 1, .1))
fun_eqvl_logistic(seq(0, 1, .1), m = 1) |> round(2) |> setNames(seq(0, 1, .1))

curve(fun_eqvl_logistic(x = x, m = 0), col = 'red'); curve(fun_eqvl(x = x, m = 0), col = 'blue', add = T); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic(x = x, m = 0.01), col = 'red'); curve(fun_eqvl(x = x, m = 0.01), col = 'blue', add = T); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic(x = x, m = 0.1), col = 'red'); curve(fun_eqvl(x = x, m = 0.1), col = 'blue', add = T); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic(x = x, m = 0.25), col = 'red'); curve(fun_eqvl(x = x, m = 0.25), col = 'blue', add = T); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic(x = x, m = 0.5), col = 'red'); curve(fun_eqvl(x = x, m = 0.5), col = 'blue', add = T); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic(x = x, m = 0.75), col = 'red'); curve(fun_eqvl(x = x, m = 0.75), col = 'blue', add = T); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic(x = x, m = 0.9), col = 'red'); curve(fun_eqvl(x = x, m = 0.9), col = 'blue', add = T); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic(x = x, m = 0.95), col = 'red'); curve(fun_eqvl(x = x, m = 0.95), col = 'blue', add = T); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic(x = x, m = 0.99), col = 'red'); curve(fun_eqvl(x = x, m = 0.99), col = 'blue', add = T); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic(x = x, m = 1), col = 'red'); curve(fun_eqvl(x = x, m = 1), col = 'blue', add = T); curve(x ^ 1, add = T)

# curve(fun_eqvl_logistic(x = x, m = 0), col = 'red'); curve(x ^ 1, add = T)
# curve(fun_eqvl_logistic(x = x, m = 0.01), col = 'red'); curve(x ^ 1, add = T)
# curve(fun_eqvl_logistic(x = x, m = 0.1), col = 'red'); curve(x ^ 1, add = T)
# curve(fun_eqvl_logistic(x = x, m = 0.25), col = 'red'); curve(x ^ 1, add = T)
# curve(fun_eqvl_logistic(x = x, m = 0.5), col = 'red'); curve(x ^ 1, add = T)
# curve(fun_eqvl_logistic(x = x, m = 0.75), col = 'red'); curve(x ^ 1, add = T)
# curve(fun_eqvl_logistic(x = x, m = 0.9), col = 'red'); curve(x ^ 1, add = T)
# curve(fun_eqvl_logistic(x = x, m = 0.95), col = 'red'); curve(x ^ 1, add = T)
# curve(fun_eqvl_logistic(x = x, m = 0.99), col = 'red'); curve(x ^ 1, add = T)
# curve(fun_eqvl_logistic(x = x, m = 1), col = 'red'); curve(x ^ 1, add = T)

# curve(fun_eqvl(x = x, t = 0), col = 'red'); curve(x ^ 1, add = T)
# curve(fun_eqvl(x = x, t = 0.25), col = 'red'); curve(x ^ 1, add = T)
# curve(fun_eqvl(x = x, t = 0.5), col = 'red'); curve(x ^ 1, add = T)
# curve(fun_eqvl(x = x, t = 0.75), col = 'red'); curve(x ^ 1, add = T)
# curve(fun_eqvl(x = x, t = 0.9), col = 'red'); curve(x ^ 1, add = T)
# curve(fun_eqvl(x = x, t = 0.95), col = 'red'); curve(x ^ 1, add = T)
# curve(fun_eqvl(x = x, t = 0.99), col = 'red'); curve(x ^ 1, add = T)
# curve(fun_eqvl(x = x, t = 1), col = 'red'); curve(x ^ 1, add = T)
 