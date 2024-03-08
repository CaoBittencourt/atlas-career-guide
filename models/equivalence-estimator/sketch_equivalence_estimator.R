fun_scale <- function(x, m){
  
  # ((1 - x) * (1 - m)) ^ - (x - m)
  # ((1 - x) * (1 - m)) ^ - (x - m)
  # 1 / m
  # (x/x) / (1 - m)
  # 1 / (1 + m)
  # 1 / ((1 - x) * (1 - m))
  # 1 / (x * (1 - m))
  # 1 / ((x / x) - m)
  # ((x/x) / (1 - m)) ^ (1 / (1 - x))
  # ((1 / m) - x) / m
  # tan(m * (pi / 2))
  tan((pi / 2) * cos(x * (1 - m)))
  # tan((pi / 2) * cos((pi / 2) * x * (1 - m)))
  # tan(acos(x * (1 - m)))
  
}

curve(fun_scale(x = x, m = 0), col = 'red', from = 0.1, to = 1); curve({tan((pi / 2) * cos(x * (1 - 0)))}, col = 'blue', add = T, from = 0.1, to = 1)
curve(fun_scale(x = x, m = 0.1), col = 'red', from = 0.1, to = 1); curve({tan((pi / 2) * cos(x * (1 - 0.1)))}, col = 'blue', add = T, from = 0.1, to = 1)
curve(fun_scale(x = x, m = 0.25), col = 'red', from = 0.1, to = 1); curve({tan((pi / 2) * cos(x * (1 - 0.25)))}, col = 'blue', add = T, from = 0.1, to = 1)
curve(fun_scale(x = x, m = 0.5), col = 'red', from = 0.1, to = 1); curve({tan((pi / 2) * cos(x * (1 - 0.5)))}, col = 'blue', add = T, from = 0.1, to = 1)
curve(fun_scale(x = x, m = 0.67), col = 'red', from = 0.1, to = 1); curve({tan((pi / 2) * cos(x * (1 - 0.67)))}, col = 'blue', add = T, from = 0.1, to = 1)
curve(fun_scale(x = x, m = 0.75), col = 'red', from = 0.1, to = 1); curve({tan((pi / 2) * cos(x * (1 - 0.75)))}, col = 'blue', add = T, from = 0.1, to = 1)
curve(fun_scale(x = x, m = 0.9), col = 'red', from = 0.1, to = 1); 
curve({tan((pi / 2) * cos(x * (1 - 0.9)))}, col = 'blue',from = 0.1, to = 1)
curve(fun_scale(x = x, m = 0.99), col = 'red', from = 0.1, to = 1); curve({tan((pi / 2) * cos(x * (1 - 0.99)))}, col = 'blue', add = T, from = 0.1, to = 1)
curve(fun_scale(x = x, m = 1), col = 'red', from = 0.1, to = 1); curve({tan((pi / 2) * cos(x * (1 - 1)))}, col = 'blue', add = T, from = 0.1, to = 1)

curve(fun_scale(x = x, m = 0), from = 0, to = 1, col = 'red')
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

(1 - .1) ^ (0.9 / (1 - 0.9))
(1 - .1) ^ (1 / 0.9)
(1 - .1) ^ 0.9
(1 - .1) ^ ((1 - 0.9) / 0.9)
(1 - .1) ^ (1 - 0.9)

fun_eqvl_logistic <- function(
    x
    , m
    , a = 0
    , b = 37
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

curve(1 / x, from = 0, to = 1)
curve(cos((1 - x) * (pi / 2)), from = 0, to = 1)
curve(exp(x), from = 0, to = 1)
curve(cos(1 - x), from = 0, to = 1)
curve(cosh(x), from = 0, to = 1)
curve(tanh((1 / (1 - x)) * (pi / 2)), from = 0, to = 1)
curve(1 / tanh(((1 - x) / x) * (pi / 2)), from = 0, to = 1)
curve(1 / tanh(((1 - x)) * (pi / 2)), from = 0, to = 1)
curve(1 / tan((1 - x) * (pi / 2)), from = 0, to = 1)
curve(1 / tan((x) * (pi / 2)), from = 0, to = 1)

fun_eqvl_logistic_trig <- function(
    x
    , m
    , a = 0
    # , b = 37
    # , b = tan(m * (pi / 2))
    # , b = tan((1 - x) * (pi / 2))
    # , b = tan(((1 - x) ^ m) * (pi / 2))
    # , b = tan((1 - x ^ m) * (pi / 2))
    # , b = tan(((1 - x) ^ (1 - m)) * (pi / 2))
    # , b = tan((1 - x * (1 - m)) * (pi / 2))

    # , b = tan((1 - x) * (pi / 2))

    # , b = tan((1 - x * (1 - m)) * (pi / 2))

    # , b = tan(((1 - x * (1 - m)) ^ (x * (1 - m))) * (pi / 2))
    # , b = tan(((1 - x * (1 - m)) ^ (x * (1 - m))) * (pi / 2))
    # , b = tan((1 - x * (1 - m)) * (pi / 2))

    # , b = tan(((1 - x * (1 - m)) ^ (1 - m)) * (pi / 2))

    # , b = tan(
    #   (pi / 2) ^ (
    #     cos((pi / 2) * (x * (1 - m)))
    #   ))

    , b = tan((pi / 2) * (cos((pi / 2) * x * (1 - m))) ^ (1 - m))
    # , b = tan((pi / 2) * (cos((pi / 2) * x * (1 - m))) ^ x)
    # , b = tan((pi / 2) * (cos((pi / 2) * x * (1 - m))) ^ m)
    
    # , b = tan((pi / 2) * (cos((pi / 2) * x * (1 - m))) ^ (1 - m))
    # , b = tan((pi / 2) * (cos((pi / 2) * x * (1 - m))) ^ ((1 - m) * x))
    # , b = tan((pi / 2) * (cos((pi / 2) * x)) ^ ((1 - m) * x))
    # , b = tan((pi / 2) * (cos((pi / 2) * (1 - m) * x)) ^ x)
    
    # , b = tan(acos((x * (1 - m)) ^ (1 / x)))
    # , b = tan(acos((x * (1 - m)) ^ (1 / m)))
    , c = 1
    , q = m * (1 - x)
    , k = x
    , nu = x / m
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

curve(fun_eqvl_logistic_trig(x = x, m = 0), col = 'red'); curve(fun_eqvl_logistic(x = x, m = 0, b = 37, nu = (1 - 0) / (1 - x)), col = 'blue', add = T); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 0.01), col = 'red'); curve(fun_eqvl_logistic(x = x, m = 0.01, b = 37, nu = (1 - 0.01) / (1 - x)), col = 'blue', add = T); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 0.1), col = 'red'); curve(fun_eqvl_logistic(x = x, m = 0.1, b = 37, nu = (1 - 0.1) / (1 - x)), col = 'blue', add = T); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 0.25), col = 'red'); curve(fun_eqvl_logistic(x = x, m = 0.25, b = 37, nu = (1 - 0.25) / (1 - x)), col = 'blue', add = T); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 0.5), col = 'red'); curve(fun_eqvl_logistic(x = x, m = 0.5, b = 37, nu = (1 - 0.5) / (1 - x)), col = 'blue', add = T); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 0.67), col = 'red'); curve(fun_eqvl_logistic(x = x, m = 0.67, b = 37, nu = (1 - 0.67) / (1 - x)), col = 'blue', add = T); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 0.75), col = 'red'); curve(fun_eqvl_logistic(x = x, m = 0.75, b = 37, nu = (1 - 0.75) / (1 - x)), col = 'blue', add = T); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 0.9), col = 'red'); curve(fun_eqvl_logistic(x = x, m = 0.9, b = 37, nu = (1 - 0.9) / (1 - x)), col = 'blue', add = T); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 0.95), col = 'red'); curve(fun_eqvl_logistic(x = x, m = 0.95, b = 37, nu = (1 - 0.95) / (1 - x)), col = 'blue', add = T); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 0.99), col = 'red'); curve(fun_eqvl_logistic(x = x, m = 0.99, b = 37, nu = (1 - 0.99) / (1 - x)), col = 'blue', add = T); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 1), col = 'red'); curve(fun_eqvl_logistic(x = x, m = 1, b = 37, nu = (1 - 1) / (1 - x)), col = 'blue', add = T); curve(x ^ 1, add = T)

curve(fun_eqvl_logistic_trig(x = x, m = 0), col = 'red'); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 0.01), col = 'red'); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 0.1), col = 'red'); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 0.25), col = 'red'); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 0.5), col = 'red'); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 0.67), col = 'red'); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 0.75), col = 'red'); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 0.9), col = 'red'); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 0.95), col = 'red'); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 0.99), col = 'red'); curve(x ^ 1, add = T)
curve(fun_eqvl_logistic_trig(x = x, m = 1), col = 'red'); curve(x ^ 1, add = T)

# curve(fun_eqvl_logistic(x = x, m = 0), col = 'red'); curve(fun_eqvl(x = x, m = 0), col = 'blue', add = T); curve(x ^ 1, add = T)
# curve(fun_eqvl_logistic(x = x, m = 0.01), col = 'red'); curve(fun_eqvl(x = x, m = 0.01), col = 'blue', add = T); curve(x ^ 1, add = T)
# curve(fun_eqvl_logistic(x = x, m = 0.1), col = 'red'); curve(fun_eqvl(x = x, m = 0.1), col = 'blue', add = T); curve(x ^ 1, add = T)
# curve(fun_eqvl_logistic(x = x, m = 0.25), col = 'red'); curve(fun_eqvl(x = x, m = 0.25), col = 'blue', add = T); curve(x ^ 1, add = T)
# curve(fun_eqvl_logistic(x = x, m = 0.5), col = 'red'); curve(fun_eqvl(x = x, m = 0.5), col = 'blue', add = T); curve(x ^ 1, add = T)
# curve(fun_eqvl_logistic(x = x, m = 0.67), col = 'red'); curve(fun_eqvl(x = x, m = 0.5), col = 'blue', add = T); curve(x ^ 1, add = T)
# curve(fun_eqvl_logistic(x = x, m = 0.75), col = 'red'); curve(fun_eqvl(x = x, m = 0.75), col = 'blue', add = T); curve(x ^ 1, add = T)
# curve(fun_eqvl_logistic(x = x, m = 0.9), col = 'red'); curve(fun_eqvl(x = x, m = 0.9), col = 'blue', add = T); curve(x ^ 1, add = T)
# curve(fun_eqvl_logistic(x = x, m = 0.95), col = 'red'); curve(fun_eqvl(x = x, m = 0.95), col = 'blue', add = T); curve(x ^ 1, add = T)
# curve(fun_eqvl_logistic(x = x, m = 0.99), col = 'red'); curve(fun_eqvl(x = x, m = 0.99), col = 'blue', add = T); curve(x ^ 1, add = T)
# curve(fun_eqvl_logistic(x = x, m = 1), col = 'red'); curve(fun_eqvl(x = x, m = 1), col = 'blue', add = T); curve(x ^ 1, add = T)

fun_eqvl_logistic_trig(seq(0, 1, .1), m = 0) |> round(2) |> setNames(seq(0, 1, .1))
fun_eqvl_logistic_trig(seq(0, 1, .1), m = 0.01) |> round(2) |> setNames(seq(0, 1, .1))
fun_eqvl_logistic_trig(seq(0, 1, .1), m = 0.1) |> round(2) |> setNames(seq(0, 1, .1))
fun_eqvl_logistic_trig(seq(0, 1, .1), m = 0.25) |> round(2) |> setNames(seq(0, 1, .1))
fun_eqvl_logistic_trig(seq(0, 1, .1), m = 0.5) |> round(2) |> setNames(seq(0, 1, .1))
fun_eqvl_logistic_trig(seq(0, 1, .1), m = 0.67) |> round(2) |> setNames(seq(0, 1, .1))
fun_eqvl_logistic_trig(seq(0, 1, .1), m = 0.75) |> round(2) |> setNames(seq(0, 1, .1))
fun_eqvl_logistic_trig(seq(0, 1, .1), m = 0.9) |> round(2) |> setNames(seq(0, 1, .1))
fun_eqvl_logistic_trig(seq(0, 1, .1), m = 0.95) |> round(2) |> setNames(seq(0, 1, .1))
fun_eqvl_logistic_trig(seq(0, 1, .1), m = 0.99) |> round(2) |> setNames(seq(0, 1, .1))
fun_eqvl_logistic_trig(seq(0, 1, .1), m = 1) |> round(2) |> setNames(seq(0, 1, .1))

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
 