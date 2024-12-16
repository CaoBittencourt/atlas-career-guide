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
getOption("atlas.occupations") |>
  read.csv() |>
  as_tibble() ->
df_occupations

# endregion
# model
# region: human capital microflexibility
df_occupations |>
  select(
    starts_with("skl_"),
    starts_with("abl_"),
    starts_with("knw_")
  ) |>
  mutate(across(
    .cols = everything(),
    .fns = ~ .x / 100
  )) |>
  kflex$phi(
    weights = df_occupations$
      employment_variants
  ) -> mtx_phi

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
