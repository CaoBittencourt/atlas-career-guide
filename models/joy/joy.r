# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  mod / compare / joy
)

library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |> readRDS() -> df_occupations
getOption("atlas.cao") |> readRDS() -> df_cao

df_cao |>
  select(item) |>
  inner_join(
    df_occupations
  ) ->
df_occupations_cao

# endregion
# model 1 (my profile)
# region: ces utility aggregator
box::use(mod / compare / joy / ugene[...])

df_cao |>
  # mutate(
  #   # cao = c(1, rep(0, 59)),
  #   # cao = rep(1, 60),
  #   # cao = rep(0, 60)
  # ) |>
  joy$agg.utility(
    df_occupations_cao,
    agg.method = "ces",
    util.fn = joy$u$quadratic
  ) |>
  arrange(desc(cao))

upsilon.gamma <- ugene(df_cao$cao)
rho <- (4 / upsilon.gamma) * (upsilon.gamma - 0.5)^2

df_occupations_cao[-1] |>
  sapply(
    function(aq) {
      sum(
        (aq / sum(aq)) * (
          mapply(
            joy$u$quadratic,
            # c(1, rep(0, 59)),
            # rep(1, 60),
            # rep(0, 60),
            # df_cao$cao,
            aq
          )^rho
        )
      )^(1 / rho)
    }
  ) |>
  as_tibble(
    rownames = "to"
  ) |>
  rename(
    cao = value
  ) |>
  arrange(desc(cao))


# endregion
# region: linear utility aggregator
df_cao |>
  joy$agg.utility(
    df_occupations_cao,
    agg.method = "linear",
    util.fn = joy$u$linear.logistic
  ) |>
  arrange(desc(cao))

# endregion
# region: convex utility aggregator
df_cao |>
  joy$agg.utility(
    df_occupations_cao,
    agg.method = "convex",
    util.fn = joy$u$logarithmic
  ) |>
  arrange(desc(cao))

# endregion
# region: concave utility aggregator
df_cao |>
  joy$agg.utility(
    df_occupations_cao,
    agg.method = "concave",
    util.fn = joy$u$logarithmic
  )

# endregion
# model 2 (occupations)
# region: ces utility aggregator
df_occupations[1:4] |>
  joy$agg.utility(
    df_occupations,
    agg.method = "ces",
    util.fn = joy$u$linear.logistic
  )
# endregion
# region: linear utility aggregator
df_occupations[1:4] |>
  joy$agg.utility(
    df_occupations,
    agg.method = "linear",
    util.fn = joy$u$linear.logistic
  )

# endregion
# region: convex utility aggregator
df_occupations[1:4] |>
  joy$agg.utility(
    df_occupations,
    agg.method = "convex",
    util.fn = joy$u$logarithmic
  )

# endregion
# region: concave utility aggregator
df_occupations[1:4] |>
  joy$agg.utility(
    df_occupations[1:9],
    agg.method = "concave",
    util.fn = joy$u$logarithmic
  )

# endregion
