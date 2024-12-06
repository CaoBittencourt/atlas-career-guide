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
library(atlas.aeq)
library(atlas.plot)

box::use(
  eq = mod / describe / eqvl
)

# endregion
# region: data
seq(0, 1, .001) |>
  as_tibble() |>
  mutate(
    aeq = eq$aeq(value, 1 - .9999999)
  ) -> df_aeq

# endregion
# region: plot
df_aeq |>
  fun_plot.line(
    aes(
      x = value,
      y = aeq
    ),
    .dbl_limits.x = c(0, 1),
    .dbl_limits.y = c(0, 1),
    .fun_format.x = percent,
    .fun_format.y = percent
  )

# endregion