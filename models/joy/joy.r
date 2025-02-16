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
# model
# region: ces utility aggregator
df_cao |>
  joy$agg.utility(
    df_occupations_cao,
    agg.method = "ces",
    util.fn = joy$u$linear.logistic
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
# region: ces utility aggregator
df_occupations[1:3] |>
  joy$agg.utility(
    df_occupations[1:4],
    agg.method = "ces",
    util.fn = function(uk, aq) uk * aq
  )


# endregion
# region: linear utility aggregator
df_occupations[1:3] |>
  joy$agg.utility(
    df_occupations[1:4],
    agg.method = "linear",
    util.fn = function(uk, aq) uk * aq
  )


# endregion
