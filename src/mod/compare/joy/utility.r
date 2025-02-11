# modular::project.options("atlas")
# region: imports
box::use(
  glogis = mod / utils / logistic,
  mod / compare / joy / ugene[...]
)

# endregion
# region: monotonic linear utility function
mono.linear <- function(uk, aq) {
  return(uk)
}

# endregion
# region: monotonic linear utility function
linear <- function(uk, aq) {
  return(
    # aq
    # ueq(uk)
    # ueq(aq)
    # ueq(uk) * ueq(aq)
    # ueq(aq)^(1 / ueq(uk))
    # uk * aq
    # ueq(uk) * aq
    # aq^(1 / ueq(uk))
    # aq^(1 / uk)

    # logistic$logistic(
    #   x = aq,
    #   a = 0,
    #   k = uk,
    #   c = 1,
    #   q = 1,
    #   m = uk,
    #   b = 1,
    #   nu = 1
    # )
  )
}

# endregion
# region: binary utility function
binary <- function(uk, aq) {
  return(aq >= uk)
}

# endregion
# region: logistic utility function
logistic <- function(uk, aq) {
  return(
    glogis$logistic(
      x = aq,
      m = 1 - uk,
      a = 0,
      k = 1,
      c = 1,
      q = 1,
      b = 1 / (1 - ugene(uk)),
      nu = 1
    )
  )
}

# endregion
# region: linear-logistic utility function
linear.logistic <- function(uk, aq) {
  x <- aq
  m <- 1 - uk

  return(
    glogis$logistic(
      x = x,
      m = m,
      a = 0,
      k = x,
      c = 1,
      q = m * (1 - x),
      nu = x / (m * (x != 1)),
      b = 1 / (1 - m)
    )
  )
}

# endregion
# region: shark fin utility function
shark.fin <- function(uk, aq) {
  return(uk)
}

# endregion
# region: logarithmic utility function
logarithmic <- function(uk, aq) {
  # return(uk * log(exp(aq)))
  return(log(exp(uk * aq)))
}

# endregion
# region: quadratic root utility function
quadratic <- function(uk, aq) {
  return((uk * aq)^2)
}

# endregion
# region: ugene root utility function
ugene.root <- function(uk, aq) {
  return((uk * aq)^(1 / ugene(uk)))
}

# endregion
# region: pref root utility function
pref.root <- function(uk, aq) {
  return(aq^(1 / uk))
}

# endregion
# # region: quadratic utility function
# quadratic <- function(uk, aq) {
#   return(1 - 4 * (aq - uk)^2)
# }

# # endregion
# region: etc utility function
linear <- function(uk, aq) {
  return(
    # aq
    # ueq(uk)
    # ueq(aq)
    # ueq(uk) * ueq(aq)
    # ueq(aq)^(1 / ueq(uk))
    # uk * aq
    # ueq(uk) * aq
    # aq^(1 / ueq(uk))
    # aq^(1 / uk)

    # logistic$logistic(
    #   x = aq,
    #   a = 0,
    #   k = uk,
    #   c = 1,
    #   q = 1,
    #   m = uk,
    #   b = 1,
    #   nu = 1
    # )
  )
}

# endregion
# region: exports
box::export(
  binary,
  logistic,
  linear.logistic,
  logarithmic,
  quadratic,
  ugene.root,
  pref.root
)

# endregion
