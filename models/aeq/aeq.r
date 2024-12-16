# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
library(dplyr)
library(tidyr)

library(atlas.plot)

box::use(
  eq = mod / describe / aeq
)

# endregion
# region: data
seq(0, 1, length.out = 8) |>
  setNames(
    seq(0, 1, length.out = 8) |> round(2)
  ) -> generality

seq(0, 1, .001) -> unit_scale

# endregion
# model
# region: attribute equivalence
generality |>
  lapply(
    function(gamma) {
      list(ã = unit_scale) |>
        as_tibble() |>
        mutate(
          aeq_linear = eq$aeq(skill_set = ã, generality = gamma, aeq_method = "linear"),
          aeq_linear_logistic = eq$aeq(skill_set = ã, generality = gamma),
          aeq_gene_root = eq$aeq(skill_set = ã, generality = gamma, aeq_method = "gene-root")
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
# plots
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
