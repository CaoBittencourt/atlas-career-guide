# setup
# region: modules
options(box.path = Sys.getenv("ATLAS_MOD"))

# endregion
# region: imports
box::use(
  stats[...],
  dplyr[...],
  tidyr[...],
  micro / kflex,
)

library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |> readRDS() -> df_occupations

# labor statistics
(Sys.getenv("ATLAS_LABOR") |> readRDS())$employment_variants -> employment

# endregion
# model
# region: human capital microflexibility
df_occupations |> kflex$microflex(employment, df_occupations$item) -> mtx_microflex

# endregion
# region: attribute dominance
df_occupations |>
  kflex$dominance.skill(
    employment,
    df_occupations$item,
    aggregate = F
  ) ->
mtx_attribute_dominance

df_occupations |>
  kflex$dominance.skill(
    employment,
    df_occupations$item
  ) ->
agg_attribute_dominance

# endregion
# plots
# region: microflexibility ordered heatmap
mtx_microflex |>
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
      title = "Human Capital Microflexibility Ordered Heatmap",
      subtitle = "Which skills contribute to learning other skills?",
      x = "",
      y = "",
      fill = "Human Capital Microflexibility",
      caption = 'Note: though attributes affect one another in learning, their "range", so to speak, is rather limited and does not form very dense clusters.\n This means attributes are not really redundant; and, consequently, the process of learning a skill is mostly "directional", with additional "indirect gains" occuring by somewhat complex causal chains.'
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
# region: microflexibility clustered heatmap
mtx_microflex |>
  as.dist() |>
  hclust() ->
hclust_microflex

mtx_microflex |>
  as_tibble(
    rownames = "to"
  ) |>
  slice(
    hclust_microflex$order
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
    .reorder_fct = F,
    .list_axis.x.args = list(
      position = "top"
    ),
    .list_geom.param = list(),
    .list_labs = list(
      title = "Human Capital Microflexibility Clustered Heatmap",
      subtitle = "Which skills contribute to learning other skills?",
      x = "",
      y = "",
      fill = "Human Capital Microflexibility",
      caption = 'Note: though attributes affect one another in learning, their "range", so to speak, is rather limited and does not form very dense clusters.\n This means attributes are not really redundant; and, consequently, the process of learning a skill is mostly "directional", with additional "indirect gains" occuring by somewhat complex causal chains.'
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
# region: attribute dominance ordered heatmap
mtx_attribute_dominance |>
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
      title = "Human Capital Dominance Ordered Heatmap",
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
      hjust = 0,
    ),
    legend.justification = "center",
    legend.title.position = "top"
  )

# endregion
# region: attribute dominance clustered heatmap
mtx_attribute_dominance |>
  as.dist() |>
  hclust() ->
hclust_dom

mtx_attribute_dominance |>
  as_tibble(
    rownames = "to"
  ) |>
  slice(
    hclust_dom$order
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
    .reorder_fct = F,
    .list_axis.x.args = list(
      position = "top"
    ),
    .list_geom.param = list(),
    .list_labs = list(
      title = "Human Capital Dominance Clustered Heatmap",
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
      hjust = 0,
    ),
    legend.justification = "center",
    legend.title.position = "top"
  )

# endregion
