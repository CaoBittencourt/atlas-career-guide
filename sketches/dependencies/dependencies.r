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
library(dplyr)

# endregion
# region: data
getOption("atlas.mod") |>
  file.path("dependencies.csv") |>
  read.csv() |>
  as_tibble() ->
df_dependencies

# endregion
# model
# region: hierarchical clustering of construct dependencies
df_dependencies |>
  select(-1) |>
  as.matrix() ->
mtx_dependencies

df_dependencies |>
  pull(1) ->
rownames(mtx_dependencies)

mtx_dependencies |>
  dist() |>
  hclust() ->
hc_dependencies

# endregion
mtx_dependencies |> kmeans(7) -> km_dependencies

box::use(
  psi = psych
)
mtx_dependencies |>
  cor() |>
  psi$fa.parallel(
    fa = "fa"
  )

mtx_dependencies |>
  psych::fa(nfactors = 5) ->
fa_dependencies

library(atlas.ftools)

atlas.ftools::fun_ftools_factor_match(fa_dependencies)

# plots
hc_dependencies |> plot()
