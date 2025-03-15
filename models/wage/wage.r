modular::project.options("atlas")
box::use(mod / utils[logis])
library(stringr)
library(dplyr)

r <- .25 / 12
1:(65 * 12) -> t
# wage.stem <- 90000
wage.stem <- mean(9000, 10000, 13000) * 12
years.stem <- 22

# wage.doctor <- 208000
# years.doctor <- 30
wage.doctor <- 7000 * 2 * 12
years.doctor <- 24

t |>
  sapply(
    logis$logistic,
    a = 0,
    k = wage.stem,
    c = 1,
    q = 1,
    m = years.stem * 12,
    b = 0.5,
    nu = 1
  ) |>
  round(2) ->
wages.engineer

t |>
  sapply(
    logis$logistic,
    a = 0,
    k = wage.doctor,
    c = 1,
    q = 1,
    m = years.doctor * 12,
    b = 0.5,
    nu = 1
  ) |>
  round(2) ->
wages.doctor

scales::dollar(sum(exp(r * (max(t) - t)) * wages.engineer))
scales::dollar(sum(exp(r * (max(t) - t)) * wages.doctor))
