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
  assert = mod / utils / assert,
  stats[weighted.mean]
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
        macroflex_weight = F
      )
  ) -> df_competence

df_competence |>
  arrange(
    competence
  )

# endregion
# region: qualification
df_occupations |>
  qa$sqa(
    df_occupations,
  ) -> df_sqa

df_occupations |>
  qa$oqa(
    df_occupations,
  ) -> df_oqa

# endregion
# region: similarity
df_occupations |>
  select(
    Anesthesiologists
  ) |>
  s$similarity(
    df_occupations,
    match_method = "cobb-douglas"
  ) -> df_similarity

# endregion
# region: evolutionary candidates
# nearest overqualified occupation
df_sqa |>
  pivot_longer(
    cols = -1,
    names_to = "from",
    values_to = "sqa"
  ) |>
  inner_join(
    df_oqa |>
      pivot_longer(
        cols = -1,
        names_to = "from",
        values_to = "oqa"
      ) |> arrange(from),
  ) |>
  select(
    from,
    into = to,
    sqa,
    oqa
  ) |>
  mutate(
    soqa = (1 - sqa) * oqa
  ) |>
  group_by(from) |>
  arrange(
    soqa,
    .by_group = T
  )


# more competent
df_competence$
  competence |>
  replicate(
    n = nrow(df_competence),
  ) |>
  t() > df_competence$
  # t() < df_competence$
  competence ->
evolution_mtx

# all(evolution_mtx |> diag() == F)

# sufficiently similar
# df_similarity[-c(1:2)]^2 -> similarity_mtx
df_similarity[
  df_similarity |>
    vapply(
      is.numeric,
      logical(1)
    )
] -> similarity_mtx

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


df_similarity |>
  pivot_longer(
    cols = -c(1:2),
    names_to = "from",
    values_to = "similarity"
  ) |>
  inner_join(
    df_competence,
    by = c("to" = "occupation")
  ) |>
  inner_join(
    df_competence |> rename(ref = competence),
    by = c("from" = "occupation")
  ) |>
  rename(
    into = to
  ) |>
  filter(
    from != into
  ) |>
  filter(
    competence > ref
  ) |>
  group_by(from) |>
  arrange(
    -similarity,
    .by_group = T
  ) |>
  slice(1) |>
  ungroup() |>
  select(
    from, into,
    similarity,
    competence,
    ref
  ) |>
  View()


# endregion
# # region: evolutionary candidates
# # more competent
# df_competence$
#   competence |>
#   replicate(
#     n = nrow(df_competence),
#   ) |>
#   t() > df_competence$
#   # t() < df_competence$
#   competence ->
# evolution_mtx

# # all(evolution_mtx |> diag() == F)

# # sufficiently similar
# # df_similarity[-c(1:2)]^2 -> similarity_mtx
# df_similarity[
#   df_similarity |>
#     vapply(
#       is.numeric,
#       logical(1)
#     )
# ] -> similarity_mtx

# similarity_mtx * (similarity_mtx >= 0.5) * evolution_mtx -> evolution_mtx

# df_competence$occupation -> colnames(evolution_mtx)
# df_competence$occupation -> rownames(evolution_mtx)

# evolution_mtx |>
#   as_tibble(
#     rownames = "into"
#   ) |>
#   pivot_longer(
#     cols = -1,
#     names_to = "from",
#     values_to = "similarity"
#   ) |>
#   group_by(from) |>
#   arrange(
#     -similarity,
#     .by_group = T
#   ) |>
#   slice(1) |>
#   select(
#     from, into,
#     similarity
#   ) |>
#   ungroup() |>
#   View()

# evolution_mtx |>
#   as_tibble(rownames = "into") |>
#   pivot_longer(
#     cols = -1,
#     names_to = "from",
#     values_to = "similarity"
#   ) |>
#   filter(
#     similarity > 0
#   ) |>
#   group_by(from) |>
#   tally() |>
#   arrange(n)

# evolution_mtx |> sapply(max)

# # all(evolution_mtx |> rownames() == df_similarity$to)
# # all(evolution_mtx |> colnames() == df_similarity[-c(1:2)] |> names())

# tibble(
#   from = df_competence$occupation,
#   into = df_competence$occupation[evolution_mtx |> sapply(which.max)]
# )


# df_similarity |>
#   pivot_longer(
#     cols = -c(1:2),
#     names_to = "from",
#     values_to = "similarity"
#   ) |>
#   inner_join(
#     df_competence,
#     by = c("to" = "occupation")
#   ) |>
#   inner_join(
#     df_competence |> rename(ref = competence),
#     by = c("from" = "occupation")
#   ) |>
#   rename(
#     into = to
#   ) |>
#   filter(
#     from != into
#   ) |>
#   filter(
#     competence > ref
#   ) |>
#   group_by(from) |>
#   arrange(
#     -similarity,
#     .by_group = T
#   ) |>
#   slice(1) |>
#   ungroup() |>
#   select(
#     from, into,
#     similarity,
#     competence,
#     ref
#   ) |>
#   View()


# # endregion
# region: evolutionary split

# endregion
