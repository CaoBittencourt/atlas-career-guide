# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  fs = mod / compare / field,
  eq = mod / describe / aeq,
  dplyr[...],
  tidyr[...],
  tibble[column_to_rownames]
)

library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |> readRDS() -> df_occupations

# endregion
# model
# region: equivalent field similarity model
df_occupations[-1] |> sapply(eq$aeq, aeq_method = "linear-logistic") -> aeq_occupations
aeq_occupations |> fs$field(aeq_occupations) -> aeq_field_mtx

# endregion
# region: field similarity model without aeq
df_occupations[-1] |> fs$field(df_occupations[-1]) -> field_mtx

# endregion
# plots
# region: equivalent field similarity matrix ordered heatmap
aeq_field_mtx |>
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
    .scale_colors = scale_fill_gradient2(
      low = "red",
      mid = "white",
      high = "blue",
      limit = c(-1, 1),
      midpoint = 0
    ),
    .reorder_fct = T,
    .reorder_fun = mean,
    .list_axis.x.args = list(
      position = "top"
    ),
    .list_geom.param = list(),
    .list_labs = list(
      title = "Equivalent Field Similarity Ordered Heatmap",
      subtitle = "Which occupations belong to the same general field of activity?",
      x = "",
      y = "",
      fill = "Equivalent Field Similarity"
      # ,
      # caption = 'Note: it is clear some attributes do contribute more to learning than others; however, no single attribute stands out as being more "dominant" overall.'
    )
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 0,
    ),
    legend.justification = "center",
    legend.title.position = "top"
  )

# endregion
# region: equivalent field similarity matrix clustered heatmap
# region: field similarity matrix ordered heatmap
field_mtx |>
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
    .scale_colors = scale_fill_gradient2(
      low = "red",
      mid = "white",
      high = "blue",
      limit = c(-1, 1),
      midpoint = 0
    ),
    .reorder_fct = T,
    .reorder_fun = mean,
    .list_axis.x.args = list(
      position = "top"
    ),
    .list_geom.param = list(),
    .list_labs = list(
      title = "Equivalent Field Similarity Ordered Heatmap",
      subtitle = "Which occupations belong to the same general field of activity?",
      x = "",
      y = "",
      fill = "Equivalent Field Similarity"
      # ,
      # caption = 'Note: it is clear some attributes do contribute more to learning than others; however, no single attribute stands out as being more "dominant" overall.'
    )
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 0,
    ),
    legend.justification = "center",
    legend.title.position = "top"
  )

# endregion
# region: field similarity matrix clustered heatmap
aeq_field_mtx |>
  as.dist() |>
  hclust() ->
hclust_aeq_field

aeq_field_mtx |>
  as_tibble(
    rownames = "to"
  ) |>
  slice(
    hclust_aeq_field$order
  ) |>
  mutate(
    to = factor(to)
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
    .scale_colors = scale_fill_gradient2(
      low = "red",
      mid = "white",
      high = "blue",
      limit = c(-1, 1),
      midpoint = 0
    ),
    .reorder_fct = T,
    .reorder_fun = mean,
    .list_axis.x.args = list(
      position = "top"
    ),
    .list_geom.param = list(),
    .list_labs = list(
      title = "Equivalent Field Similarity Ordered Heatmap",
      subtitle = "Which occupations belong to the same general field of activity?",
      x = "",
      y = "",
      fill = "Equivalent Field Similarity"
      # ,
      # caption = 'Note: it is clear some attributes do contribute more to learning than others; however, no single attribute stands out as being more "dominant" overall.'
    )
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 0,
    ),
    legend.justification = "center",
    legend.title.position = "top"
  )

# endregion