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
vertex.to |> pa$path(vertex.from) -> epath

# career path
occupations[epath |> pa$which.path()]

# path cost
pa$path.cost(epath) |> sum()

# base cost
vertex.to |> pa$vertex.cost()

# path efficiency
epath |> pa$path.efficiency()

# verify path is optimal
if (
  pa$path.efficiency(epath) >= 0
) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pa$path.efficiency(epath), 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pa$path.efficiency(epath), 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: accountant => financial managers
# occupations
occupation.from <- occupations$`Accountants and Auditors`
occupation.to <- occupations$`Financial Managers`

# vertices
occupation.from |> pa$match.vertex() -> vertex.from
occupation.to |> pa$match.vertex() -> vertex.to

# find path
vertex.to |> pa$path(vertex.from) -> epath

# career path
occupations[epath |> pa$which.path()]

# path cost
pa$path.cost(epath) |> sum()

# base cost
vertex.to |> pa$vertex.cost()

# path efficiency
epath |> pa$path.efficiency()

# verify path is optimal
if (
  pa$path.efficiency(epath) >= 0
) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pa$path.efficiency(epath), 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pa$path.efficiency(epath), 2),
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
vertex.to |> pa$path(vertex.from) -> epath

# career path
occupations[epath |> pa$which.path()]

# path cost
pa$path.cost(epath) |> sum()

# base cost
vertex.to |> pa$vertex.cost()

# path efficiency
epath |> pa$path.efficiency()

# verify path is optimal
if (
  pa$path.efficiency(epath) >= 0
) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pa$path.efficiency(epath), 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pa$path.efficiency(epath), 2),
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
vertex.to |> pa$path(vertex.from) -> epath

# career path
occupations[epath |> pa$which.path()]

# path cost
pa$path.cost(epath) |> sum()

# base cost
vertex.to |> pa$vertex.cost()

# path efficiency
epath |> pa$path.efficiency()

# verify path is optimal
if (
  pa$path.efficiency(epath) >= 0
) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pa$path.efficiency(epath), 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pa$path.efficiency(epath), 2),
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
vertex.to |> pa$path(vertex.from) -> epath

# career path
occupations[epath |> pa$which.path()]

# path cost
pa$path.cost(epath) |> sum()

# base cost
vertex.to |> pa$vertex.cost()

# path efficiency
epath |> pa$path.efficiency()

# verify path is optimal
if (
  pa$path.efficiency(epath) >= 0
) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pa$path.efficiency(epath), 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pa$path.efficiency(epath), 2),
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
vertex.to |> pa$path(vertex.from) -> epath

# career path
occupations[epath |> pa$which.path()]

# path cost
pa$path.cost(epath) |> sum()

# base cost
vertex.to |> pa$vertex.cost()

# path efficiency
epath |> pa$path.efficiency()

# verify path is optimal
if (
  pa$path.efficiency(epath) >= 0
) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pa$path.efficiency(epath), 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pa$path.efficiency(epath), 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: economists => statisticians
# occupations
occupation.from <- occupations$Economists
occupation.to <- occupations$Statisticians

# vertices
occupation.from |> pa$match.vertex() -> vertex.from
occupation.to |> pa$match.vertex() -> vertex.to

# find path
vertex.to |> pa$path(vertex.from) -> epath

# career path
occupations[epath |> pa$which.path()]

# path cost
pa$path.cost(epath) |> sum()

# base cost
vertex.to |> pa$vertex.cost()

# path efficiency
epath |> pa$path.efficiency()

# verify path is optimal
if (
  pa$path.efficiency(epath) >= 0
) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pa$path.efficiency(epath), 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pa$path.efficiency(epath), 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: statisticians => economists
# occupations
occupation.from <- occupations$Statisticians
occupation.to <- occupations$Economists

# vertices
occupation.from |> pa$match.vertex() -> vertex.from
occupation.to |> pa$match.vertex() -> vertex.to

# find path
vertex.to |> pa$path(vertex.from) -> epath

# career path
occupations[epath |> pa$which.path()]

# path cost
pa$path.cost(epath) |> sum()

# base cost
vertex.to |> pa$vertex.cost()

# path efficiency
epath |> pa$path.efficiency()

# verify path is optimal
if (
  pa$path.efficiency(epath) >= 0
) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pa$path.efficiency(epath), 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pa$path.efficiency(epath), 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
