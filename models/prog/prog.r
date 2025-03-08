# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use()

library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |> readRDS() -> df_occupations

# squared similarity matrix = temp employability matrix
"atlas.root" |>
  getOption() |>
  file.path(
    "models",
    "evol",
    "similarity.rds"
  ) |>
  readRDS() ->
mtx_similarity

# labor stats
getOption("atlas.labor") |> readRDS() -> df_labor

# max number of job progressions
stages <- 7

# endregion
# model
# region: employability matrix
# squared similarity matrix = temp employability matrix
mtx_similarity[-1] |>
  pivot_longer(
    cols = -1,
    names_to = "from",
    values_to = "employability"
  ) |>
  mutate(
    employability = employability^2,
    employability = employability * (employability > 0.5)
  ) ->
df_prog

# endregion
# region: progression 1x1 combinations
df_prog |>
  inner_join(
    df_labor |>
      rename(
        employment.to = employment_variants,
        wage.to = wage
      ),
    by = c(
      "to" = "occupation"
    )
  ) |>
  inner_join(
    df_labor |>
      rename(
        employment.from = employment_variants,
        wage.from = wage
      ),
    by = c(
      "from" = "occupation"
    )
  ) |>
  select(
    from, to,
    employability,
    employment.to,
    employment.from,
    wage.to,
    wage.from
  ) ->
df_prog

# endregion
# region: career stages (wage brackets)
df_prog |>
  filter(
    wage.from <= wage.to
  ) |>
  mutate(
    wage.diff = wage.to - wage.from
  ) |>
  group_by(from) |>
  mutate(
    stage =
      findInterval(
        wage.diff,
        seq(
          wage.diff |> min(),
          wage.diff |> max(),
          length.out = stages + 1
        ),
        left.open = T
      )
  ) ->
df_prog

# endregion
# region: valid career progressions (yk <= yq, Wkq > 0)
df_prog |>
  filter(
    employability > 0
  ) ->
df_prog

# endregion
# region: progression probability and expected payoff
df_prog |>
  group_by(from, stage) |>
  mutate(
    # probability of finding each job at each stage of career progression
    # note 0 <= sum(prob) <= 1 for each from, stage group
    # thus, because 0 <= employability <= 1, probs don't have to sum to 1
    prob = employability * employment.to / sum(employment.to)
    # prob = employability
  ) |>
  ungroup() |>
  select(
    stage,
    from,
    to,
    prob,
    wage.to = wage.to
  ) |>
  group_by(from, stage) |>
  arrange(-prob, -wage.to, .by_group = T) |>
  slice(1) |>
  ungroup() |>
  filter(
    from == first(from)
  )
# |>
#   group_by(stage) |>
#   group_split()





# endregion

# region: dsds
mtx_similarity |>
  select(
    to,
    employability = `Accountants and Auditors`
  ) |>
  mutate(
    employability = employability^4,
    employability = employability * (employability > 0.5)
  ) |>
  inner_join(
    df_labor |>
      select(
        to = occupation,
        employment.to = employment_variants,
        wage.to = wage,
        wage.bracket.to = wage.bracket
      )
  ) |>
  mutate(
    wage.from =
      df_labor |>
        filter(
          occupation == "Accountants and Auditors"
        ) |>
        pull(wage),
    wage.bracket.from =
      df_labor |>
        filter(
          occupation == "Accountants and Auditors"
        ) |>
        pull(wage) |>
        findInterval(
          seq(
            df_labor$wage |> min(),
            df_labor$wage |> max(),
            length.out = 11
          )
        )
  ) |>
  filter(
    wage.from <= wage.to
  ) |>
  filter(
    employability > 0
  ) |>
  group_by(wage.bracket.to) |>
  mutate(
    employment.bracket = sum(employment.to),
    prob = employability * employment.to / sum(employment.to)
  ) |>
  select(
    stage, from, to,
    prob,
    wage = wage.to
  )
arrange(
  -wage.to * prob,
  .by_group = T
) |>
  slice(1) |>
  ungroup() |>
  arrange(
    wage.to
  )

# endregion

# region: career progression candidates (yk <= yq)
df_prog |>
  filter(
    wage.from < wage.to
  ) |>
  arrange(
    from,
    -employability,
    -wage.to
  )

# endregion
# region: career progression brackets (yb = yq)

# endregion
# region: valid career progressions (yk <= yq, Wkq > 0)

# endregion
# region: expected payoff for each career path

# endregion
# region: expected payoff overall

# endregion
