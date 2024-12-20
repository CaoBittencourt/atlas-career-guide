# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  dom = mod / compare / dom,
  kflex = mod / micro / kflex,
  dplyr[...],
  tidyr[...],
  tibble[column_to_rownames]
)

library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |>
  readRDS() |>
  column_to_rownames("occupation") |>
  as.matrix() ->
skill_mtx

# labor statistics
getOption("atlas.labor") |>
  readRDS() ->
df_labor

# check if matches
all(rownames(skill_mtx) == df_labor$occupation)

# endregion
# model
# region: human capital microflexibility matrix
skill_mtx |> kflex$phi(df_labor$employment_norm) -> mtx_phi

# endregion
# region: attribute dominance matrix
mtx_phi |> dom$dominance(aggregate = F) -> mtx_dom

# endregion
# region: aggregate attribute dominance
mtx_phi |> dom$dominance() -> dominance

# endregion
# tests
# region: dominance diagonal is zero
all(mtx_dom |> diag() == 0)

# endregion
# plots
# region: attribute dominance matrix heatmap
mtx_dom |>
  as_tibble(
    rownames = "from"
  ) |>
  pivot_longer(
    cols = -1,
    names_to = "to"
  ) |>
  fun_plot.heatmap(
    aes(
      x = from,
      y = to,
      fill = value
    ),
    .list_geom.param = list(),
    .reorder_desc = T,
    .reorder_fun = max
  )

# endregion
