# modular::project.options("atlas")
# region: imports
box::use(
  glogis = mod / utils / logistic,
  mod / compare / joy / ugene[...]
)

# endregion
# region: monotonic linear utility function
linear <- function(uk, aq) {
  return(uk * aq)
}

# endregion
# region: binary utility function
binary <- function(uk, aq) {
  return(uk >= aq)
}

# endregion
# region: logistic utility function
logistic <- function(uk, aq) {
  return(
    glogis$logistic(
      x = aq,
      m = 1 - uk,
      a = 0,
      k = uk,
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
  return(
    (aq^(uk / aq)) / (
      1 + (1 - uk / aq) * exp(
        (-40 / uk) * (-aq + uk)
      )
    )
    # ^(aq / uk)
  )
}

# endregion
# region: linear shark fin utility function
shark.linear <- function(uk, aq) {
  return(
    aq / (
      1 + (1 - uk / aq) * exp(
        (-40 / uk) * (-aq + uk)
      )
    )
    # ^(aq / uk)
  )
}

# endregion
# region: linear-logistic shark fin utility function
shark.llogis <- function(uk, aq) {
  return(
    (aq^(uk / aq)) / (
      1 + (1 - uk / aq) * exp(
        (-40 / uk) * (-aq + uk)
      )
    )
    # ^(aq / uk)
  )
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
  return(
    ifelse(
      all(uk == 0, aq == 0),
      1,
      max(
        0, 1 - ((aq - uk) / uk)^2
      )
    )
  )
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
# region: roof utility function
roof <- function(uk, aq) {
  return(1 - abs(uk - aq))
}

# endregion
# region: scaled roof utility function
roof.scaled <- function(uk, aq) {
  # return(roof(uk, aq)^(1 / ugene(uk)))
  return(roof(uk, aq)^(1 / 0.4466))
}

# endregion
# region: leontief utility function
leontief <- function(uk, aq) {
  # return(pmin(uk, aq))
  # return(pmin(uk * uk, uk * aq))
  return(sqrt(pmin(uk * uk, uk * aq)))
}

# endregion
# region: leontief scaled utility function
leontief.scaled <- function(uk, aq) {
  # return(leontief(uk, aq)^(1 / ugene(uk)))
  return(leontief(uk, aq)^(1 / .4466))
}

# endregion
# # region: quadratic utility function
# quadratic <- function(uk, aq) {
#   return(1 - 4 * (aq - uk)^2)
# }

# # endregion
# region: exports
list(
  "binary" = binary,
  "linear" = linear,
  "logistic" = logistic,
  "linear.logistic" = linear.logistic,
  "logarithmic" = logarithmic,
  "quadratic" = quadratic,
  "ugene.root" = ugene.root,
  "pref.root" = pref.root,
  "roof" = roof,
  "roof.scaled" = roof.scaled,
  "leontief" = leontief,
  "leontief.scaled" = leontief.scaled,
  "shark.fin" = shark.fin,
  "shark.linear" = shark.linear,
  "shark.llogis" = shark.llogis
) -> u

box::export(u)

# endregion
