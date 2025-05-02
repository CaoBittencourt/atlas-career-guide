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

index.from <- 1
index.to <- 1

# find path
pa$path(
  from = vertices.from$vertex[index.from],
  to = vertices.to$vertex[index.to]
) -> path.actor_accountant

# career path
occupations[
  pa$paths$table |>
    slice(
      path.actor_accountant
    ) |>
    pull(occupation)
]

# verify path is optimal
if (
  path.actor_accountant |>
    pa$path.cost() |>
    sum() <=
    sum(
      vertices.to$x[index.to],
      vertices.to$t[index.to]
    )
) {
  print("Path is optimal.")
} else {
  print("Path is not optimal.")
}

# endregion
# region: accountant => actor
# get vertices
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
vertices.from

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
vertices.to

index.from <- 1
index.to <- 10

# find path
pa$path(
  from = vertices.from$vertex[index.from],
  to = vertices.to$vertex[index.to]
) -> path.actor_accountant

# career path
occupations[
  pa$paths$table |>
    slice(
      path.actor_accountant
    ) |>
    pull(occupation)
]

# verify path is optimal
if (
  path.actor_accountant |>
    pa$path.cost() |>
    sum() <=
    sum(
      vertices.to$x[index.to],
      vertices.to$t[index.to]
    )
) {
  print("Path is optimal.")
} else {
  print("Path is not optimal.")
}

# endregion
