# setup
# region: imports
library(ipeadatar)
library(lubridate)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)

# endregion
# region: data
# default discount rate
discountDefault <- .05

# rents table
read.csv(
  url(
    "https://docs.google.com/spreadsheets/d/e/2PACX-1vSMtreiEOjVAORxhEwcZk5DIJC1XEI5t3Q2FzXlHbTxGwx_ZCrjsWJ35igbku2NbXn1lfBfsSh7E6fR/pub?gid=670718951&single=true&output=csv"
  ),
  sep = ",",
  dec = ","
) |>
  as_tibble() |>
  mutate(
    across(
      .cols = starts_with("date"),
      .fns = as.Date,
      format = "%d/%m/%Y"
    )
  ) ->
rents

# recurring hazards table
read.csv(
  url(
    "https://docs.google.com/spreadsheets/d/e/2PACX-1vSMtreiEOjVAORxhEwcZk5DIJC1XEI5t3Q2FzXlHbTxGwx_ZCrjsWJ35igbku2NbXn1lfBfsSh7E6fR/pub?gid=0&single=true&output=csv"
  ),
  sep = ",",
  dec = ","
) |>
  as_tibble() |>
  mutate(
    across(
      .cols = starts_with("date"),
      .fns = as.Date,
      format = "%d/%m/%Y"
    ),
    dateSolved = if_else(
      is.na(dateSolved),
      today(),
      dateSolved
    ),
    discount = if_else(
      is.na(discount),
      discountDefault,
      discount
    )
  ) ->
hazardsRecurring

# lumpsum (non recurring) hazards table
read.csv(
  url(
    "https://docs.google.com/spreadsheets/d/e/2PACX-1vSMtreiEOjVAORxhEwcZk5DIJC1XEI5t3Q2FzXlHbTxGwx_ZCrjsWJ35igbku2NbXn1lfBfsSh7E6fR/pub?gid=1650970469&single=true&output=csv"
  ),
  sep = ",",
  dec = ","
) |>
  as_tibble() |>
  mutate(
    across(
      .cols = starts_with("date"),
      .fns = as.Date,
      format = "%d/%m/%Y"
    ),
    dateEnd = if_else(
      is.na(dateEnd),
      today(),
      dateEnd
    )
  ) ->
hazardsLumpsum

# tibble(
#   "hazardId" = 1:2 |> as.factor(),
#   "type" = c("cupim", "infiltração") |> as.factor(),
#   "apt" = c((5 * 4)1, 405) |> as.factor(),
#   "desc" = c("discountDefault", "lalala"),
#   "dateNotified" = c("2025-01-01", "2024-07-01") |> as.Date(),
#   "dateSolved" = c("2025-03-01", "2025-03-01") |> as.Date(),
#   "discount" = c(.05, .1),
# ) -> hazards

# risk-free interest rate (selic)
read.csv(
  url(
    paste0(
      "https://api.bcb.gov.br/dados/serie/bcdata.sgs.11/dados?formato=csv&dataInicial=",
      min(
        hazardsRecurring$dateNotified |> min(na.rm = T),
        hazardsLumpsum$dateStart |> min(na.rm = T)
      ) |> format(format = "%d/%m/%Y"),
      "&dataFinal=",
      max(
        hazardsRecurring$dateSolved |> max(na.rm = T),
        hazardsLumpsum$dateEnd |> max(na.rm = T)
      ) |> format(format = "%d/%m/%Y")
    )
  ),
  sep = ";",
  dec = ","
) |>
  as_tibble() |>
  rename(
    date = 1,
    interestRate = 2
  ) |>
  mutate(
    across(
      .cols = starts_with("date"),
      .fns = as.Date,
      format = "%d/%m/%Y"
    ),
    interestRate = 1 + interestRate / 100,
  ) ->
interestRate

# net condominium fee (minus baseline water expenditure = 90)
condo.fee <- 240 - 90

# interestRate |>
#   reframe(
#     timeDiff =
#       as.integer(
#         max(date) - min(date)
#       ),
#     timeNrowDiff =
#       timeDiff - n()
#   )

# # inflation rate
# # ipeadatar::available_series() |>
# #   mutate(across(
# #     .cols = -where(is.numeric),
# #     .fns = str_to_lower
# #   )) |>
# #   filter(
# #     # name |> str_detect("ipca") &
# #     name |> str_detect("selic") &
# #       freq == "monthly" &
# #       status == "active"
# #   ) |>
# #   arrange(desc(
# #     lastupdate
# #   )) -> inflationSeries

# "precos12_ipcag12" |>
#   str_to_upper() |>
#   ipeadata() |>
#   rename(
#     inflation = value
#   ) |>
#   select(
#     date,
#     inflation
#   ) |>
#   filter(
#     date >=
#       hazards$
#         dateNotified |>
#         min() - months(1)
#   ) |>
#   filter(
#     date <=
#       hazards$
#         dateSolved |>
#         max()
#   ) |>
#   mutate(
#     inflation = 1 + inflation / 100
#   ) |>
#   slice(-1) ->
# inflation

# endregion
# model
# region: illegal condominium fee compensation
rents |>
  filter(
    !is.na(dateFirstCondoFee)
  ) |>
  mutate(
    dateDiff = as.integer(
      today() |>
        format(
          "%Y-%m-01"
        ) |>
        as.Date() -
        dateFirstCondoFee
    )
  ) |>
  group_by(apt) |>
  slice(
    rep(1, dateDiff)
  ) |>
  mutate(
    date = dateFirstCondoFee + days(row_number() - 1)
  ) |>
  ungroup() |>
  inner_join(
    interestRate,
    multiple = "all"
  ) |>
  group_by(apt) |>
  mutate(
    interestCum =
      interestRate |>
        rev() |>
        cumprod() |>
        rev(),
    compensation =
      condo.fee *
        interestCum / (5 * 4)
  )
group_by(apt) |>
  reframe(
    compensation = sum(compensation),
  )


# endregion
# region: recurring (discount) compensation
hazardsRecurring |>
  mutate(
    dateDiff =
      as.integer(
        dateSolved - dateNotified
      )
  ) |>
  filter(
    !is.na(dateDiff)
  ) |>
  group_by(hazardId, apt) |>
  slice(
    rep(1, dateDiff)
  ) |>
  mutate(
    date = dateNotified + days(row_number() - 1)
  ) |>
  ungroup() |>
  inner_join(
    interestRate,
    multiple = "all"
  ) |>
  group_by(hazardId, apt) |>
  mutate(
    interestCum =
      interestRate |>
        rev() |>
        cumprod() |>
        rev()
  ) |>
  inner_join(
    rents,
    multiple = "all"
  ) |>
  group_by(hazardId, apt) |>
  mutate(
    compensation =
      rentBonified *
        discount *
        interestCum / (5 * 4)
  ) |>
  select(
    hazardId,
    apt,
    compensation
  ) |>
  group_by(hazardId, apt) |>
  reframe(
    compensation = sum(compensation),
  ) ->
compensationRecurring

compensationRecurring |>
  group_by(apt) |>
  reframe(
    compensationRecurring = sum(compensation)
  )

sum(compensationRecurring$compensation, 2 * 250, 2 * 500, 2 * 500, 500, (240 - 90) * 9)

# endregion
# # region: compensation
# hazards |>
#   group_by(hazardId) |>
#   mutate(
#     monthDiff =
#       dateNotified |>
#         interval(
#           dateSolved
#         ) %/% months(1) |>
#         as.integer()
#   ) |>
#   slice(
#     rep(1, monthDiff)
#   ) |>
#   mutate(
#     date = dateNotified + months(row_number() - 1)
#   ) |>
#   ungroup() |>
#   inner_join(
#     inflation,
#     multiple = "all"
#   ) |>
#   group_by(hazardId) |>
#   mutate(
#     inflationCum =
#       inflation |>
#         rev() |>
#         cumprod() |>
#         rev()
#   ) |>
#   inner_join(
#     rents,
#     multiple = "all"
#   ) |>
#   mutate(
#     compensation =
#       rentBonified *
#         discount *
#         inflationCum
#   ) |>
#   select(
#     -rentId,
#     -rentBonified
#   ) ->
# hazards

# hazards

# hazards |>
#   group_by(apt) |>
#   reframe(
#     compensationTotal =
#       sum(compensation)
#   )

# # endregion
