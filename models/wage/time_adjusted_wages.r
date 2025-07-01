# setup
# region: modules
options(box.path = Sys.getenv("ATLAS_MOD"))

# endregion
# region: imports
box::use(
  utils[logis],
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
getOption("atlas.labor") |>
  readRDS() |>
  inner_join(
    # bls clusters
    Sys.getenv("ATLAS_OLD_DATA") |>
      read.csv() |>
      select(
        occupation,
        bls_cluster = career_cluster
      )
  ) -> df_labor

# endregion
# model
# region: parameters
# interest rate (monthly continuously compounded)
# r <- .1 / 12
r <- .07 / 12
# r <- .05 / 12

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
    bls_cluster,
    education,
    education_years,
    retirement,
    wage,
    lifetime.earnings,
    employment = employment_variants
  ) |>
  mutate(
    wage.adjusted =
      lifetime.earnings *
        exp(
          -12 * (retirement - min(education_years)) * r
        ) / 12
  ) ->
df_lifetime

# endregion
# results
# region: careers with highest adjusted lifetime earnings
df_lifetime |>
  arrange(
    -lifetime.earnings
  ) |>
  select(
    occupation,
    bls_cluster,
    education,
    lifetime.earnings,
    employment
  )

print('p.s.: database bug in "Makeup Artists, Theatrical and Performance"; wages are not 100k, but rather 40k.')

# endregion
# region: careers with lowest adjusted lifetime earnings
df_lifetime |>
  arrange(
    lifetime.earnings
  ) |>
  select(
    occupation,
    bls_cluster,
    education,
    lifetime.earnings,
    employment
  )

# endregion
# region: highest nominal wages vs highest real wages
bind_rows(
  df_lifetime |>
    arrange(
      -wage
    ) |>
    slice(1:10),
  df_lifetime |>
    arrange(
      -wage.adjusted
    ) |>
    slice(1:10)
) |>
  arrange(
    -wage.adjusted
  ) |>
  select(
    -education_years,
    -retirement
  )


# endregion
# plots
# region: bls clusters lifetime earnings distribution
df_lifetime |>
  fun_plot.density(
    aes(
      x = lifetime.earnings,
      weights = employment
    ),
    .sym_facets = bls_cluster,
    .reorder_desc = T,
    .list_geom.param = list(
      fill = "#290396",
      alpha = 0.75,
      bw = df_lifetime$lifetime.earnings |> min()
    ),
    .list_axis.x.args = list(
      limits = c(
        0, max(df_lifetime$lifetime.earnings) * 1.1
      )
    ),
    .fun_format.x = function(x) {
      x |>
        dollar(
          accuracy = 1L,
          scale = 1 / 1000000,
          suffix = "M"
        )
    }
  )

# endregion
# region: log-normal wages simulation
df_lifetime |>
  group_by(bls_cluster) |>
  reframe(
    log.wages.mean = Hmisc::wtd.mean(
      log(wage.adjusted),
      employment
    ),
    log.wages.sd = Hmisc::wtd.var(
      log(wage.adjusted),
      employment
    ) |> sqrt()
  ) ->
df_wages.lnorm.stats

Map(
  function(cluster, logmean, logsd) {
    rlnorm(10000, logmean, logsd)
  },
  cluster = df_wages.lnorm.stats$bls_cluster,
  logmean = df_wages.lnorm.stats$log.wages.mean,
  logsd = df_wages.lnorm.stats$log.wages.sd
) |>
  bind_rows(
    .id = "bls_cluster"
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "bls_cluster",
    values_to = "wage.adjusted"
  ) ->
df_wages.lnorm

df_wages.lnorm |>
  fun_plot.ridges(
    aes(
      x = wage.adjusted,
      y = bls_cluster
    ),
    .fun_format.x = function(x) {
      x |>
        dollar(
          accuracy = 1L,
          scale = 1 / 1000,
          suffix = "K"
        )
    },
    .fun_format.y = function(y) {
      y
    }
  )

# endregion
