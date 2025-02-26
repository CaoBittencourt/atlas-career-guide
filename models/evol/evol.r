# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  s = mod / compare / match,
  qa = mod / compare / qa,
  c = mod / describe / comp,
  mod / utils / conform[...],
  vec = mod / compare / match / methods / vec,
  assert = mod / utils / assert
)

library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |> readRDS() -> df_occupations
getOption("atlas.skills") |> readRDS() -> df_occupations_long

# endregion
# model
# region: competence
df_occupations_long |>
  group_by(occupation) |>
  reframe(
    competence =
      c$comp(
        item_score,
        comp_method = "cobb-douglas"
      )
  ) -> df_competence

# endregion
# region: qualification
df_occupations |>
  qa$sqa(
    df_occupations
  ) -> df_similarity

# endregion
# # region: similarity
# df_occupations |>
#   s$similarity(
#     df_occupations,
#     match_method = "cobb-douglas"
#   ) -> df_similarity

# # endregion
# region: evolutionary candidates
# more competent
df_competence$
  competence |>
  replicate(
    n = nrow(df_competence),
  ) |>
  t() < df_competence$
  competence ->
evolution_mtx

# all(evolution_mtx |> diag() == F)

# sufficiently similar
df_similarity[-c(1:2)]^2 -> similarity_mtx

similarity_mtx * (similarity_mtx >= 0.5) * evolution_mtx -> evolution_mtx

df_competence$occupation -> colnames(evolution_mtx)
df_competence$occupation -> rownames(evolution_mtx)

evolution_mtx |>
  as_tibble(
    rownames = "into"
  ) |>
  pivot_longer(
    cols = -1,
    names_to = "from",
    values_to = "similarity"
  ) |>
  group_by(from) |>
  arrange(
    -similarity,
    .by_group = T
  ) |>
  slice(1) |>
  select(
    from, into,
    similarity
  ) |>
  ungroup() |>
  View()

evolution_mtx |>
  as_tibble(rownames = "into") |>
  pivot_longer(
    cols = -1,
    names_to = "from",
    values_to = "similarity"
  ) |>
  filter(
    similarity > 0
  ) |>
  group_by(from) |>
  tally() |>
  arrange(n)

evolution_mtx |> sapply(max)

# all(evolution_mtx |> rownames() == df_similarity$to)
# all(evolution_mtx |> colnames() == df_similarity[-c(1:2)] |> names())

tibble(
  from = df_competence$occupation,
  into = df_competence$occupation[evolution_mtx |> sapply(which.max)]
)


# endregion
# region: evolutionary split

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
