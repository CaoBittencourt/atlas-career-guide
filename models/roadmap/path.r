# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  pa = mod / roadmap / path,
  dplyr[...],
  stats[setNames]
)

# library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |>
  readRDS() |>
  select(-1) |>
  names() ->
occupations

occupations |>
  seq_along() |>
  as.list() |>
  setNames(
    occupations
  ) ->
occupations

# endregion
# model
# region: actor => accountant
# get vertices
pa$paths$table |>
  filter(
    occupation ==
      occupations$Actors
  ) |>
  select(
    occupation,
    vertex,
    x,
    t
  ) |>
  unique() |>
  arrange(x, t) ->
vertices.from

pa$paths$table |>
  filter(
    occupation ==
      occupations$`Accountants and Auditors`
  ) |>
  select(
    occupation,
    vertex,
    x,
    t
  ) |>
  unique() |>
  arrange(x, t) ->
vertices.to

# find path
pa$path(
  from = vertices.from$vertex[1],
  to = vertices.to$vertex[1]
) -> path.actor_accountant

occupations[
  pa$paths$table |>
    slice(
      path.actor_accountant
    ) |>
    pull(occupation)
]

# endregion
