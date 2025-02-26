# setup
# region: imports
library(ipeadatar)
library(lubridate)
library(stringr)
library(dplyr)
library(tidyr)

# endregion
# region: data
# rents table
tibble(
  "rentId" = 1:2 |> as.factor(),
  "apt" = c(301, 405) |> as.factor(),
  "rentBonified" = c(1500, 1100)
) -> rents

# dates and discounts table
tibble(
  "hazardId" = 1:2 |> as.factor(),
  "type" = c("cupim", "infiltração") |> as.factor(),
  "apt" = c(301, 405) |> as.factor(),
  "desc" = c("dsds", "lalala"),
  "dateNotified" = c("2025-01-01", "2024-07-01") |> as.Date(),
  "dateSolved" = c("2025-03-01", "2025-03-01") |> as.Date(),
  "discount" = c(.05, .1),
) -> hazards



# inflation rate
# ipeadatar::available_series() |>
#   mutate(across(
#     .cols = -where(is.numeric),
#     .fns = str_to_lower
#   )) |>
#   filter(
#     name |> str_detect("ipca") &
#       freq == "monthly" &
#       status == "active"
#   ) |>
#   arrange(desc(
#     lastupdate
#   )) -> inflationSeries

"precos12_ipcag12" |>
  str_to_upper() |>
  ipeadata() |>
  rename(
    inflation = value
  ) |>
  select(
    date,
    inflation
  ) |>
  filter(
    date >=
      hazards$
        dateNotified |>
        min() - months(1)
  ) |>
  filter(
    date <=
      hazards$
        dateSolved |>
        max()
  ) |>
  mutate(
    inflation = 1 + inflation / 100
  ) |>
  slice(-1) ->
inflation

# endregion
# model
# region: parameters

# endregion
# region: compensation
hazards |>
  group_by(hazardId) |>
  mutate(
    monthDiff =
      dateNotified |>
        interval(
          dateSolved
        ) %/% months(1) |>
        as.integer()
  ) |>
  slice(
    rep(1, monthDiff)
  ) |>
  mutate(
    date = dateNotified + months(row_number() - 1)
  ) |>
  ungroup() |>
  inner_join(
    inflation,
    multiple = "all"
  ) |>
  group_by(hazardId) |>
  mutate(
    inflationCum =
      inflation |>
        rev() |>
        cumprod() |>
        rev()
  ) |>
  inner_join(
    rents,
    multiple = "all"
  ) |>
  mutate(
    compensation =
      rentBonified *
        discount *
        inflationCum
  ) |>
  select(
    -rentId,
    -rentBonified
  ) ->
hazards

hazards

hazards |>
  group_by(apt) |>
  reframe(
    compensationTotal =
      sum(compensation)
  )

# endregion
# # exp(.01)
# # 1.01^12
# # log(1.12)

# # sum(1.01^(0:8) * 1500 * .3) * 8 + sum(1.01^(0:11) * (240 - 90)) * 18
# # sum(1.01^(0:8) * 1500 * .3) + sum(1.01^(0:11) * (240 - 90))
# # sum(1.01^(0:11) * (240 - 90))
# # sum(exp(0.1133287 * 0:8 / 12) * 1500 * .3) * 8 + sum(exp(0.1133287 * 0:11 / 12) * (240 - 90)) * 18

# # sum(exp(0.1133287 * 0:8 / 12) * 1100 * .5) + sum(exp(0.1133287 * 0:11 / 12) * (240 - 90))

# # monthly interest rate
# ipca <- 1.0968
# log(ipca) / 12 -> i

# # affected time periods (months)
# t <- 9

# # total time periods (months)
# tt <- 12

# # average rent (brl)
# rent <- 1500

# # average discount on rent (%)
# discount <- .25

# # average net condominium fee (minus baseline water expenditure = 90)
# condo.fee <- 240 - 90

# # total number of households
# n <- 18

# # number of affected households (8?)
# m <- 8

# sum(exp(i * 0:(t - 1)) * rent * discount) * m + sum(exp(i * 0:(tt - 1)) * condo.fee) * n
