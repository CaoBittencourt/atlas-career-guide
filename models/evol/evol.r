# setup
# region: modules
options(box.path = Sys.getenv("ATLAS_MOD"))

# endregion
# region: imports
box::use(
  s = compare / similarity,
  qa = compare / qa,
  c = describe / comp,
  utils / conform[...],
  assert = utils / assert,
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
        c$comp.methods$expertise
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

df_sqa |>
  readr::write_rds(
    file.path(
      "models",
      "evol",
      "sqa.rds"
    )
  )

# df_occupations |>
#   qa$oqa(
#     df_occupations,
#   ) -> df_oqa

# endregion
# region: similarity
df_occupations |>
  s$similarity(
    df_occupations,
    match_method = "cobb-douglas"
  ) -> df_similarity

df_similarity |>
  readr::write_rds(
    file.path(
      "models",
      "evol",
      "similarity.rds"
    )
  )

# endregion
# region: evolutionary candidates
df_similarity |>
  pivot_longer(
    cols = -c(1:2),
    names_to = "from",
    values_to = "similarity"
  ) |>
  inner_join(
    df_competence |>
      rename(
        from = occupation,
        competence.from = competence
      )
  ) |>
  inner_join(
    df_competence |>
      rename(
        to = occupation,
        competence.to = competence
      )
  ) |>
  filter(
    similarity > 0.5
  ) |>
  filter(
    competence.from < competence.to
  ) |>
  group_by(to) |>
  arrange(
    desc(similarity),
    .by_group = T
  ) |>
  slice(1) |>
  ungroup() |>
  relocate(
    method,
    from, to,
    where(is.numeric)
  ) ->
df_evol

df_evol |>
  filter(
    from ==
      df_competence |>
        arrange(competence) |>
        slice(1) |>
        pull(occupation)
  )

df_evol |>
  filter(
    from ==
      "Gambling and Sports Book Writers and Runners"
  )

df_evol |>
  filter(
    from ==
      "Tellers"
  )

df_evol |>
  filter(
    from ==
      "Cashiers"
  )

df_evol |>
  filter(
    from ==
      "Receptionists and Information Clerks"
  )

df_evol |>
  filter(
    from ==
      "Manicurists and Pedicurists"
  )

# # note: the least competent occupation cannot evolve from any occupation
# df_competence |> arrange(competence)
# df_similarity$to[!(df_similarity$to %in% df_evol$to)]


# endregion
# # region: evolutionary candidates
# df_sqa |>
#   pivot_longer(
#     cols = -1,
#     names_to = "from",
#     values_to = "sqa.from"
#   ) |>
#   inner_join(
#     df_competence |>
#       rename(
#         from = occupation,
#         competence.from = competence
#       )
#   ) |>
#   inner_join(
#     df_competence |>
#       rename(
#         to = occupation,
#         competence.to = competence
#       )
#   ) |>
#   filter(
#     competence.from < competence.to
#   ) |>
#   group_by(to) |>
#   arrange(
#     desc(sqa.from),
#     .by_group = T
#   ) |>
#   slice(1) |>
#   ungroup() |>
#   View()


# # endregion
# # region: evolutionary candidates
# # nearest overqualified occupation
# df_sqa |>
#   pivot_longer(
#     cols = -1,
#     names_to = "from",
#     values_to = "sqa"
#   ) |>
#   inner_join(
#     df_oqa |>
#       rename(from = to) |>
#       pivot_longer(
#         cols = -1,
#         names_to = "to",
#         values_to = "oqa"
#       ),
#   ) |>
#   select(
#     from,
#     into = to,
#     sqa,
#     oqa
#   ) |>
#   mutate(
#     soqa = (1 - sqa) * oqa
#   ) |>
#   group_by(from) |>
#   arrange(
#     soqa,
#     .by_group = T
#   )


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
