# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  mod / compare / joy,
  mod / compare / joy / ugene
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
# box::use(mod / compare / joy / ugene[...])
df_cao |>
  mutate(
    # cao = c(1, rep(0, 59)),
    cao = c(1, .00001, rep(0, 58)),
    # cao = rep(1, 60),
    # cao = rep(0, 60)
  ) |>
  # reframe(
  #   ugene$ugene(cao)
  # ) |>
  joy$agg.utility(
    df_occupations_cao,
    agg.method = "convex",
    util.fn = joy$u$logistic
  ) |>
  arrange(desc(cao))

# endregion
# # region: test
# box::use(
#   assert = mod / utils / assert[...],
#   mod / compare / joy / ugene[...],
#   mod / utils / conform[...],
# )

# df_cao |> assert$as.skill_mtx() -> Uk
# df_occupations_cao |> assert$as.skill_mtx() -> A
# Uk |> conform(A) -> Uk

# upsilon.gamma <- ugene(Uk[[1]][[1]])
# rho <- (4 / upsilon.gamma) * (upsilon.gamma - 0.5)^2

# # apply utility function
# joy$u$quadratic |> mapply(Uk$cao, A) -> Uk

# # ces utility aggregator
# colSums(
#   (A / colSums(A)) * (Uk^rho)
# )^(1 / rho)

# df_occupations_cao[-1] |>
#   sapply(
#     function(aq) {
#       sum(
#         (aq / sum(aq)) * (
#           mapply(
#             joy$u$quadratic,
#             c(1, rep(0, 59)),
#             # rep(1, 60),
#             # rep(0, 60),
#             # df_cao$cao,
#             aq
#           )^rho
#         )
#       )^(1 / rho)
#     }
#   ) |>
#   as_tibble(
#     rownames = "to"
#   ) |>
#   rename(
#     cao = value
#   ) |>
#   arrange(desc(cao))

# # endregion
# region: ces utility aggregator
df_cao |>
  joy$agg.utility(
    df_occupations_cao,
    agg.method = "ces",
    util.fn = joy$u$leontief
  ) |>
  arrange(desc(cao))

# endregion
# region: linear utility aggregator
df_cao |>
  joy$agg.utility(
    df_occupations_cao,
    agg.method = "linear",
    util.fn = joy$u$shark.llogis
  ) |>
  arrange(desc(cao))

# endregion
# region: convex utility aggregator
df_cao |>
  joy$agg.utility(
    df_occupations_cao,
    agg.method = "convex",
    util.fn = joy$u$shark.llogis
  ) |>
  arrange(desc(cao))

# endregion
# region: concave utility aggregator
df_cao |>
  joy$agg.utility(
    df_occupations_cao,
    agg.method = "concave",
    util.fn = joy$u$shark.llogis
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
