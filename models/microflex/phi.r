# setup
# region: modules
# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

# install modular if not installed
if (!any(utils::installed.packages()[, 1] == "modular")) {
  devtools::install_github("CaoBittencourt/modular")
}

library(modular)

# objective project root
project.options(
  project.name = "atlas",
  relative.paths = list(
    atlas.src = "src",
    atlas.mod = "src/mod",
    box.path = "src",
    atlas.data = "data"
  ),
  root.name = ".atlas"
)

# endregion
# region: imports
box::use(
  dplyr[...],
  tidyr[...],
  kflex = sketch / micro / kflex / microflex,
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
    rownames = 'from'
  ) |> 
  pivot_longer
fun_plot.heatmap()

# endregion
# r-styler
# CaoBittencourt/modular