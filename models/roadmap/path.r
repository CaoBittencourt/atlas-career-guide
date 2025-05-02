# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  pa = mod / roadmap / path,
  gr = igraph,
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
# occupations
occupation.from <- occupations$Actors
occupation.to <- occupations$`Accountants and Auditors`

# vertices
occupation.from |> pa$match.vertex() -> vertex.from
occupation.to |> pa$match.vertex() -> vertex.to

# find path
vertex.to |> pa$path(vertex.from) -> path.actor_accountant

# career path
occupations[path.actor_accountant |> pa$which.path()]

# path cost
pa$path.cost(path.actor_accountant) |> sum()

# base cost
vertex.to |> pa$vertex.cost()

# path efficiency
path.actor_accountant |> pa$path.efficiency()

# verify path is optimal
if (
  pa$path.efficiency(path.actor_accountant) >= 0
) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pa$path.efficiency(path.actor_accountant), 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pa$path.efficiency(path.actor_accountant), 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: actor => musician
# occupations
occupation.from <- occupations$Actors
occupation.to <- occupations$`Musicians and Singers`

# vertices
occupation.from |> pa$match.vertex() -> vertex.from
occupation.to |> pa$match.vertex() -> vertex.to

# find path
vertex.to |> pa$path(vertex.from) -> path.actor_musician

# career path
occupations[path.actor_musician |> pa$which.path()]

# path cost
pa$path.cost(path.actor_musician) |> sum()

# base cost
vertex.to |> pa$vertex.cost()

# path efficiency
path.actor_musician |> pa$path.efficiency()

# verify path is optimal
if (
  pa$path.efficiency(path.actor_musician) >= 0
) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pa$path.efficiency(path.actor_musician), 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pa$path.efficiency(path.actor_musician), 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: actor => art director
# occupations
occupation.from <- occupations$Actors
occupation.to <- occupations$`Art Directors`

# vertices
occupation.from |> pa$match.vertex() -> vertex.from
occupation.to |> pa$match.vertex() -> vertex.to

# find path
vertex.to |> pa$path(vertex.from) -> path.actor_art_director

# career path
occupations[path.actor_art_director |> pa$which.path()]

# path cost
pa$path.cost(path.actor_art_director) |> sum()

# base cost
vertex.to |> pa$vertex.cost()

# path efficiency
path.actor_art_director |> pa$path.efficiency()

# verify path is optimal
if (
  pa$path.efficiency(path.actor_art_director) >= 0
) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pa$path.efficiency(path.actor_art_director), 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pa$path.efficiency(path.actor_art_director), 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: accountant => accountant
# occupations
occupation.from <- occupations$`Accountants and Auditors`
occupation.to <- occupations$`Accountants and Auditors`

# vertices
occupation.from |> pa$match.vertex() -> vertex.from
occupation.to |> pa$which.vertex("max") -> vertex.to

# find path
vertex.to |> pa$path(vertex.from) -> path.accountant_accountant

# career path
occupations[path.accountant_accountant |> pa$which.path()]

# path cost
pa$path.cost(path.accountant_accountant) |> sum()

# base cost
vertex.to |> pa$vertex.cost()

# path efficiency
path.accountant_accountant |> pa$path.efficiency()

# verify path is optimal
if (
  pa$path.efficiency(path.accountant_accountant) >= 0
) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pa$path.efficiency(path.accountant_accountant), 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pa$path.efficiency(path.accountant_accountant), 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: actor => actuary
# occupations
occupation.from <- occupations$Actors
occupation.to <- occupations$Actuaries

# vertices
occupation.from |> pa$match.vertex() -> vertex.from
occupation.to |> pa$match.vertex() -> vertex.to

# find path
vertex.to |> pa$path(vertex.from) -> path.actor_actuary

# career path
occupations[path.actor_actuary |> pa$which.path()]

# path cost
pa$path.cost(path.actor_actuary) |> sum()

# base cost
vertex.to |> pa$vertex.cost()

# path efficiency
path.actor_actuary |> pa$path.efficiency()

# verify path is optimal
if (
  pa$path.efficiency(path.actor_actuary) >= 0
) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pa$path.efficiency(path.actor_actuary), 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pa$path.efficiency(path.actor_actuary), 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
