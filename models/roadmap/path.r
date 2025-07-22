# setup
# region: modules
options(box.path = Sys.getenv("ATLAS_MOD"))

# endregion
# region: imports
box::use(
  pa = roadmap / path,
  gr = igraph,
  dplyr[...],
  stats[setNames]
)

# library(atlas.plot)

# endregion
# region: data
# skill set matrix
# Sys.getenv("ATLAS_SKILLS") |>
#   readRDS() |>
#   filter(
#     occupation %in%
#       (Sys.getenv("ATLAS_EDUCATION") |>
#         readRDS() |>
#         filter(education_years <= 17) |>
#         pull(occupation))
#   ) |>
#   inner_join(
#     Sys.getenv("ATLAS_LABOR") |>
#       readRDS()
#   ) |>
#   group_by(item) |>
#   reframe(
#     item_score = stats::weighted.mean(
#       item_score,
#       employment_norm
#     )
#   ) -> basicEducation

Sys.getenv("ATLAS_SKILLS_MTX") |>
  readRDS() |>
  select(-1) |>
  names() |>
  c("Basic Education") -> occupations

occupations |>
  seq_along() |>
  as.list() |>
  setNames(
    occupations
  ) -> occupations

# endregion
# model
# # simplified model
# region: actor => musician
# occupations
occupations$Actors -> occupation.from
occupations$`Musicians and Singers` -> occupation.to

# find path
occupation.from |> pa$path(occupation.to) -> epath

# career path
epath |>
  pa$path.timeline() |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$path.cost(epath) |> sum() -> pathCost

# base cost
occupations$`Basic Education` |>
  pa$path(occupation.to) |>
  pa$path.cost() -> baseCost

# path efficiency
epath |> pa$path.efficiency()

pa$paths$expected$vertices |>
  filter(
    career == 874
  ) |>
  slice(1) |>
  pull(vertex) |>
  pa$path(
    epath[[1]]
  ) |>
  path.cost() |>
  sum()

# verify path is optimal
if (pathEfficiency >= 0) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pathEfficiency, 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pathEfficiency, 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: actor => art director
# occupations
occupations$Actors -> occupation.from
occupations$`Art Directors` -> occupation.to

# find path
occupation.to |>
  pa$path(
    occupation.from,
    graph = pa$paths$expected$graph
  ) -> epath

# career path
epath |>
  pa$path.timeline(
    graph = pa$paths$expected$graph
  ) |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$path.cost(epath, pa$paths$expected$graph) |> sum() -> pathCost

# base cost
occupation.to |>
  pa$path(
    occupations$`Basic Education`,
    graph = pa$paths$expected$graph
  ) |>
  pa$path.cost(
    pa$paths$expected$graph
  ) -> baseCost

# path efficiency
1 - pathCost / baseCost -> pathEfficiency
# epath |> pa$path.efficiency()

# verify path is optimal
if (pathEfficiency >= 0) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pathEfficiency, 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pathEfficiency, 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: actor => accountant
# occupations
occupations$Actors -> occupation.from
occupations$`Accountants and Auditors` -> occupation.to

# find path
occupation.to |>
  pa$path(
    occupation.from,
    graph = pa$paths$expected$graph
  ) -> epath

# career path
epath |>
  pa$path.timeline(
    graph = pa$paths$expected$graph
  ) |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$path.cost(epath, pa$paths$expected$graph) |> sum() -> pathCost

# base cost
occupation.to |>
  pa$path(
    occupations$`Basic Education`,
    graph = pa$paths$expected$graph
  ) |>
  pa$path.cost(
    pa$paths$expected$graph
  ) -> baseCost

# path efficiency
1 - pathCost / baseCost -> pathEfficiency
# epath |> pa$path.efficiency()

# verify path is optimal
if (pathEfficiency >= 0) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pathEfficiency, 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pathEfficiency, 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: financial analysts => accountant
# occupations
occupations$`Credit Analysts` -> occupation.from
occupations$`Civil Engineers` -> occupation.to

# find path
occupation.to |>
  pa$path(
    occupation.from,
    graph = pa$paths$expected$graph
  ) -> epath

# career path
epath |>
  pa$path.timeline(
    graph = pa$paths$expected$graph
  ) |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$path.cost(epath, pa$paths$expected$graph) |> sum() -> pathCost

# base cost
occupation.to |>
  pa$path(
    occupations$`Basic Education`,
    graph = pa$paths$expected$graph
  ) |>
  pa$path.cost(
    pa$paths$expected$graph
  ) -> baseCost

# path efficiency
1 - pathCost / baseCost -> pathEfficiency
# epath |> pa$path.efficiency()

# verify path is optimal
if (pathEfficiency >= 0) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pathEfficiency, 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pathEfficiency, 2),
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
occupation.from |> pa$match.vertex(0, 0) -> vertex.from
pa$paths$vertices |>
  filter(occupation == occupation.to) |>
  arrange(-x, -t) |>
  filter(prob > 0) |>
  slice(1) |>
  pull(vertex) -> vertex.to
# occupation.to |> pa$match.vertex(Inf, Inf) -> vertex.to

# find path
vertex.to |> pa$path(vertex.from) -> epath
epath

# career path
epath |>
  pa$path.timeline() |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$path.cost(epath) |> sum()

# base cost
vertex.to |> pa$vertex.cost()

# path efficiency
epath |> pa$path.efficiency()

# verify path is optimal
if (pa$path.efficiency(epath) >= 0) {
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
# region: actor => accountant
# occupations
occupation.from <- occupations$Actors
occupation.to <- occupations$`Accountants and Auditors`

# vertices
occupation.from |> pa$match.vertex(0, 0) -> vertex.from
pa$paths$vertices |>
  filter(occupation == occupation.to) |>
  arrange(-x, -t) |>
  filter(prob > 0) |>
  slice(1) |>
  pull(vertex) -> vertex.to
# occupation.to |> pa$match.vertex(Inf, Inf) -> vertex.to

# find path
vertex.to |> pa$path(vertex.from) -> epath
epath

# career path
epath |>
  pa$path.timeline() |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$path.cost(epath) |> sum()

# base cost
vertex.to |> pa$vertex.cost()

# path efficiency
epath |> pa$path.efficiency()

# verify path is optimal
if (pa$path.efficiency(epath) >= 0) {
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
# region: high school => accountant
# occupations
occupation.from <- occupations$`Basic Education`
occupation.to <- occupations$`Accountants and Auditors`

# vertices
occupation.from |> pa$match.vertex(0, 0) -> vertex.from
# pa$paths$vertices |>
#   filter(occupation == occupation.to) |>
#   arrange(-x, -t) |>
#   filter(prob > 0) |>
#   slice(1) |>
#   pull(vertex) ->
# vertex.to
occupation.to |> pa$match.vertex(0, 0) -> vertex.to

# find path
vertex.to |> pa$path(vertex.from) -> epath
epath

# career path
epath |>
  pa$path.timeline() |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$path.cost(epath) |> sum()

# base cost
vertex.to |> pa$vertex.cost()

# path efficiency
epath |> pa$path.efficiency()

# verify path is optimal
if (pa$path.efficiency(epath) >= 0) {
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
epath

# career path
epath |>
  pa$path.timeline() |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$path.cost(epath) |> sum()

# base cost
vertex.to |> pa$vertex.cost()

# path efficiency
epath |> pa$path.efficiency()

# verify path is optimal
if (pa$path.efficiency(epath) >= 0) {
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
epath

# career path
epath |>
  pa$path.timeline() |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$path.cost(epath) |> sum()

# base cost
vertex.to |> pa$vertex.cost()

# path efficiency
epath |> pa$path.efficiency()

# verify path is optimal
if (pa$path.efficiency(epath) >= 0) {
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
epath

# career path
epath |>
  pa$path.timeline() |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$path.cost(epath) |> sum()

# base cost
vertex.to |> pa$vertex.cost()

# path efficiency
epath |> pa$path.efficiency()

# verify path is optimal
if (pa$path.efficiency(epath) >= 0) {
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
# # detailed model
