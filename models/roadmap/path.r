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
  names() |>
  c("Basic Education") ->
occupations

occupations |>
  seq_along() |>
  as.list() |>
  setNames(
    occupations
  ) ->
occupations

getOption("atlas.mod") |>
  file.path(
    "roadmap",
    "path",
    "data",
    "rds",
    "vertices.rds"
  ) |>
  readRDS() ->
from.to.vertices

# endregion
# model
# region: actor => musician
# occupations
occupation.from <- occupations$Actors
occupation.to <- occupations$`Musicians and Singers`

# vertices
occupation.from |> pa$match.vertex(0, 0) -> vertex.from
pa$paths$vertices |>
  filter(occupation == occupation.to) |>
  arrange(-x, -t) |>
  slice(1) |>
  pull(vertex) ->
vertex.to
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
occupation.from |> pa$match.vertex(0, 0) -> vertex.from
pa$paths$vertices |>
  filter(occupation == occupation.to) |>
  arrange(-x, -t) |>
  filter(prob > 0) |>
  slice(1) |>
  pull(vertex) ->
vertex.to
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
  pull(vertex) ->
vertex.to
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
# tests
# # region: sampling
# from.to.vertices |>
#   slice_sample(n = 20) ->
# from.to.vertices.sample

# from.to.vertices.sample |>
#   mutate(
#     cost =
#       mapply(
#         function(from, to) {
#           to |>
#             pa$path(from) |>
#             pa$path.cost() |>
#             sum()
#         },
#         from = vertex,
#         to = vertex.to
#       )
#   ) |>
#   mutate(
#     from = names(occupations)[from] |> substring(1, 50),
#     to = names(occupations)[to] |> substring(1, 50),
#     years.saved = default.cost - cost
#   ) |>
#   select(
#     -starts_with("vertex")
#   ) |>
#   arrange(
#     -years.saved,
#     -default.cost
#   ) ->
# from.to.model

# from.to.model |> print(width = 1000)

# # endregion
# # region: from scratch (basic education)
# from.to.vertices |>
#   filter(from == 874) |>
#   slice_sample(n = 20) |>
#   mutate(
#     cost =
#       mapply(
#         function(from, to) {
#           to |>
#             pa$path(from) |>
#             pa$path.cost() |>
#             sum()
#         },
#         from = vertex,
#         to = vertex.to
#       )
#   ) |>
#   mutate(
#     from = names(occupations)[from] |> substring(1, 50),
#     to = names(occupations)[to] |> substring(1, 50),
#     years.saved = default.cost - cost
#   ) |>
#   select(
#     -starts_with("vertex")
#   ) |>
#   arrange(-years.saved) ->
# from.to.model

# from.to.model

# from.to.model |>
#   reframe(
#     all(
#       (cost - default.cost) <= 0
#     )
#   )

# # endregion
