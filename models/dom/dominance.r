# setup
# region: modules
options(box.path = Sys.getenv("ATLAS_MOD"))

# endregion
# region: imports
box::use(
  dom = compare / dom,
  # kflex = micro / kflex,
  dplyr[...],
  tidyr[...],
  tibble[column_to_rownames]
)

library(atlas.plot)

# endregion
# region: data
# similarity matrix
# Sys.getenv("ATLAS_ROOT") |>
#   file.path(
#     "articles",
#     "1.introduction-matching",
#     "output",
#     "list_matching.rds"
#   ) |>
#   readRDS() ->
# skill_mtx

# labor statistics
Sys.getenv("ATLAS_LABOR") |>
  readRDS() ->
df_labor

# check if matches
all(rownames(skill_mtx) == df_labor$occupation)

# endregion
# model
# region: human capital microflexibility matrix
skill_mtx |> kflex$microflex(df_labor$employment_norm) -> mtx_microflex

# endregion
# region: attribute dominance matrix
mtx_microflex |> dom$dominance(aggregate = F) -> mtx_dom

# endregion
# region: aggregate attribute dominance
mtx_microflex |> dom$dominance() -> dominance

dominance

# endregion
# tests
# region: dominance diagonal is zero
all(mtx_dom |> diag() == 0)

# endregion
# plots
# region: attribute dominance matrix heatmap
mtx_dom |>
  as_tibble(
    rownames = "to"
  ) |>
  pivot_longer(
    cols = -1,
    names_to = "from"
  ) |>
  fun_plot.heatmap(
    aes(
      x = from,
      y = to,
      fill = value
    ),
    .reorder_fct = F,
    .list_geom.param = list(),
    .list_labs = list(
      title = "Attribute Dominance Heatmap",
      subtitle = "Which skills contribute to learning other skills?",
      x = "",
      y = "",
      fill = "Human Capital Dominance",
      caption = 'Note: it is clear some attributes do contribute more to learning than others; however, no single attribute stands out as being more "dominant" overall.'
    )
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1,
    ),
    legend.justification = "center",
    legend.title.position = "top"
  )

# endregion
