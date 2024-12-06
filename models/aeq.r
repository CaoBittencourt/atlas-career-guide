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
library(tidyr)

library(atlas.plot)

box::use(
  eq = mod / describe / eqvl,
)

# endregion
# region: data
seq(0, 1, length.out = 8) |>
  setNames(
    seq(0, 1, length.out = 8) |> round(2)
  ) |>
  lapply(
    function(gamma) {
      list(ã = seq(0, 1, .001)) |>
        as_tibble() |>
        mutate(
          aeq_linear = eq$aeq(skill_set = ã, generality = gamma, method = "linear"),
          aeq_linear_logistic = eq$aeq(skill_set = ã, generality = gamma),
          aeq_specialty_root = eq$aeq(skill_set = ã, generality = gamma, method = "specialty-root")
        )
    }
  ) |>
  bind_rows(
    .id = "generality"
  ) |>
  mutate(
    generality = as.ordered(
      generality
    )
  ) |>
  pivot_longer(
    cols = -c(1, 2),
    names_to = "metric",
    values_to = "aeq"
  ) -> df_aeq

# endregion
# region: plot
df_aeq |>
  fun_plot.line(
    aes(
      x = ã,
      y = aeq,
      color = metric,
      group = metric
    ),
    .dbl_limits.x = c(0, 1),
    .dbl_limits.y = c(0, 1),
    .fun_format.x = percent,
    .fun_format.y = percent,
    .sym_facets = generality,
    .int_facets = 4
  )

# endregion
