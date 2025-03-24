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
  tibble[column_to_rownames],
  mod / utils / egmap[...]
)

library(atlas.plot)

# endregion
# region: data
# skill set matrix
(getOption("atlas.skills_mtx") |> readRDS())[2:8] -> df_occupations
df_occupations |> sapply(eq$aeq, aeq_method = "linear-logistic") -> aeq_occupations

# endregion
# model
# region: cosine equivalent field similarity model
# calculate equivalent field similarity matrix
aeq_occupations |> fs$field(aeq_occupations, fs$field.methods$cosine) -> aeq_field_mtx

aeq_field_mtx

# note: field similarity is symmetric
all((aeq_field_mtx |> lower.tri()) == t(aeq_field_mtx |> upper.tri()))

# note: field similarity is really quite high
aeq_field_mtx[aeq_field_mtx |> lower.tri(diag = F)] |> summary()

# getOption("atlas.oldata") |>
#   read.csv() |>
#   pull(career_cluster) |>
#   unique() |>
#   length() ->
# nclusters

# aeq_field_mtx |> kmeans(nclusters) -> kmeans_field

# tibble(
#   occupation = kmeans_field$cluster |> names(),
#   cluster = kmeans_field$cluster |> factor(levels = seq_len(nclusters))
# ) |>
#   group_by(cluster) |>
#   group_split()

# endregion
# region: cosine field similarity model without aeq
# note: attribute equivalence helps to differentiate occupations' fields
df_occupations |> fs$field(df_occupations, fs$field.methods$cosine) -> field_mtx

field_mtx

sum(field_mtx >= aeq_field_mtx) / length(field_mtx)
mean(field_mtx - aeq_field_mtx)
max(field_mtx - aeq_field_mtx)
min(field_mtx - aeq_field_mtx)

# endregion
# region: cobb-douglas equivalent field similarity model
# attribute equivalence
# calculate equivalent field similarity matrix
aeq_occupations |> fs$field(aeq_occupations, fs$field.methods$cobb_douglas) -> aeq_field_mtx

aeq_field_mtx

# note: field similarity is symmetric
all((aeq_field_mtx |> lower.tri()) == t(aeq_field_mtx |> upper.tri()))

# note: field similarity is really quite high
aeq_field_mtx[aeq_field_mtx |> lower.tri(diag = F)] |> summary()

# getOption("atlas.oldata") |>
#   read.csv() |>
#   pull(career_cluster) |>
#   unique() |>
#   length() ->
# nclusters

# aeq_field_mtx |> kmeans(nclusters) -> kmeans_field

# tibble(
#   occupation = kmeans_field$cluster |> names(),
#   cluster = kmeans_field$cluster |> factor(levels = seq_len(nclusters))
# ) |>
#   group_by(cluster) |>
#   group_split()

# endregion
# region: cobb-douglas field similarity model without aeq
# note: attribute equivalence helps to differentiate occupations' fields
df_occupations |> fs$field(df_occupations, fs$field.methods$cobb_douglas) -> field_mtx

field_mtx

sum(field_mtx >= aeq_field_mtx) / length(field_mtx)
mean(field_mtx - aeq_field_mtx)
max(field_mtx - aeq_field_mtx)
min(field_mtx - aeq_field_mtx)

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
      limit = c(0, 1),
      midpoint = 0.5
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
      limit = c(0, 1),
      midpoint = 0.5
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
