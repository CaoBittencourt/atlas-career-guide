# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  dplyr[...],
  tidyr[...],
  kflex = mod / micro / kflex / microflex,
)

library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |> readRDS() -> df_occupations

# labor statistics
getOption("atlas.labor") |>
  readRDS() |>
  inner_join(
    df_occupations
  ) -> df_labor

df_labor$employment_norm |>
  setNames(df_labor$occupation) ->
employment_levels

# endregion
# model
# region: human capital microflexibility
df_occupations[-1] |> kflex$phi(employment_levels) -> mtx_phi

# endregion
# plots
# region: microflexibility heatmap
mtx_phi |>
  as_tibble(
    rownames = "from"
  ) |>
  pivot_longer(
    cols = -1,
    names_to = "to",
    values_to = "phi"
  ) |>
  fun_plot.heatmap(
    aes(
      x = from,
      y = to,
      fill = phi
    ),
    .reorder_desc = F,
    .reorder_fun = min,
    .list_geom.param = list()
  )

# endregion
