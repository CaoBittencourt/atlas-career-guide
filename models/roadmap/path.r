# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  pa = mod / roadmap / path,
  gr = igraph,
  dplyr[...],
  tidyr[...],
)

# library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |> readRDS() -> df_occupations

# endregion
# model
# region: sketch
pa$paths$table |>
  filter(
    occupation == 1,
    occupation.to == 1
  ) ->
vertices

pa$path(1L, 6L) -> epath

epath

df_occupations[-1][
  pa$paths$graph |>
    gr$get.edge.attribute(
      "occupation.to",
      epath
    )
]

epath |> pa$path.cost()

epath |>
  pa$path.cost() |>
  sum()

pa$paths$graph
pa$path.util
pa$path.cost


# endregion
