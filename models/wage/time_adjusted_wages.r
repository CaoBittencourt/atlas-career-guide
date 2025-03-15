# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  mod / utils[logis],
  dplyr[...],
)

library(atlas.plot)

# endregion
# region: data
# education stats
getOption("atlas.education") |>
  readRDS() |>
  mutate(
    education_years =
      ifelse(
        education == "Doctoral or professional degree",
        28,
        education_years
      )
  ) -> df_edu

# labor stats
getOption("atlas.labor") |> readRDS() -> df_labor

# endregion
# model
# region: parameters
# interest rate (monthly continuously compounded)
# r <- .1 / 12
r <- .07 / 12

# retirement age
t <- 65

# endregion
# region: monthly earnings timeline
wage.month <- function(wage.year, years.min, years.max) {
  return(
    1:(years.max * 12) |>
      sapply(
        logis$logistic,
        a = 0,
        k = wage.year / 12,
        c = 1,
        q = 1,
        m = (years.min + 1) * 12,
        b = 0.25,
        nu = 1
      ) |>
      round(2)
  )
}

# endregion
# region: lifetime earnings
lifetime.earnings <- function(wage.year, years.min, years.max, interest.rate) {
  return(
    sum(
      exp(
        interest.rate * ((12 * years.max):0)[-1]
      ) * wage.month(
        wage.year,
        years.min,
        years.max
      )
    )
  )
}

# endregion
# region: interest adjusted lifetime earnings
df_edu |>
  inner_join(
    df_labor
  ) |>
  mutate(
    retirement = t,
    interest.rate = r,
    lifetime.earnings = Map(
      lifetime.earnings,
      wage,
      education_years,
      retirement,
      interest.rate
    ) |> unlist()
  ) -> df_lifetime

df_lifetime |>
  select(
    occupation,
    education,
    education_years,
    retirement,
    wage,
    lifetime.earnings
  ) |>
  arrange(
    -lifetime.earnings
  ) |>
  print(n = 20)

# endregion
