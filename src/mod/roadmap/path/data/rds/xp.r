# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  readr[...],
  dplyr[...],
  tidyr[...],
  stringr[...],
  readxl[read_excel],
)

# endregion
# region: data
# similarity matrix
getOption("atlas.mod") |>
  file.path(
    "roadmap",
    "path",
    "data",
    "rds",
    "similarity.rds"
  ) |>
  readRDS() ->
df_similarity

# occupations' ids and soc codes
df_similarity$to |>
  as_tibble() |>
  rename(
    occupation = 1
  ) |>
  mutate(
    .before = 1,
    id = row_number()
  ) |>
  inner_join(
    getOption("atlas.oldata") |>
      read.csv() |>
      select(
        id_soc_code,
        occupation
      ) |>
      arrange(
        occupation
      ) |>
      mutate(
        .before = 1,
        id = row_number()
      )
  ) |>
  relocate(
    -occupation
  ) ->
df_ids

# requirements
getOption("atlas.data") |>
  file.path("education_training_experience.xlsx") |>
  read_excel() ->
df_req

getOption("atlas.data") |>
  file.path("education_training_experience_categories.xlsx") |>
  read_excel() ->
df_req_cat

# endregion
# model
# region: dsds
# get closest match
df_similarity |>
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
  slice(-1) |>
  slice(1) |>
  ungroup() |>
  inner_join(
    df_ids |> rename_with(.fn = ~ .x |> paste0(".to")),
    by = c(
      "to" = "occupation.to"
    )
  ) |>
  inner_join(
    df_ids |> rename_with(.fn = ~ .x |> paste0(".from")),
    by = c(
      "from" = "occupation.from"
    )
  ) ->
df_closest_match

df_req |>
  mutate(
    .before = 1,
    id_soc_code =
      `O*NET-SOC Code` |>
        str_sub(1, -4)
  ) |>
  pull(id_soc_code) |>
  # pull(`O*NET-SOC Code`) |>
  unique() |>
  length()

inner_join(
  df_ids
)


# endregion
