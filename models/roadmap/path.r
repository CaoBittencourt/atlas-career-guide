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
# find path
occupations$`Accountants and Auditors` |>
  pa$match.vertex() |>
  pa$path(
    from = occupations$Actors |> pa$match.vertex()
  ) ->
path.actor_accountant

pa$paths$graph |> gr$get.edge.attribute("occupation.from", path.actor_accountant)
pa$paths$graph |> gr$get.edge.attribute("occupation.to", path.actor_accountant)

box::use(mod / roadmap / path / functions / base_cost[...])
1 - (path.actor_accountant |> pa$path.cost() |> sum()) / (occupations$`Accountants and Auditors` |> pa$match.vertex() |> vertex.cost())

pa$paths$vertices |> filter(occupation == 2)

# # career path
# occupations[
#   pa$paths$table |>
#     filter(
#       vertex %in%
#         path.actor_accountant
#     ) |>
#     pull(occupation)
# ]

# verify path is optimal
if (
  path.actor_accountant |>
    pa$path.cost() |>
    sum() <=
    sum(
      pa$paths$vertices[
        pa$paths$vertices$vertex == pa$match.vertex(occupations$`Accountants and Auditors`),
        c("x", "t")
      ]
    )
) {
  print("Path is optimal.")
} else {
  print("Path is not optimal.")
}

# path cost
path.actor_accountant |>
  pa$path.cost() |>
  sum()

# path efficiency
path.actor_accountant |> pa$path.efficiency()

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
  to = vertices.to$vertex[index.to],
  from = vertices.from$vertex[index.from]
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
