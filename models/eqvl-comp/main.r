# setup
# region: modules
modular::project.options('atlas')

# endregion
# region: imports
box::use(
  assert = mod / utils / assert,
  gn = mod / describe / gene,
  dplyr[...],
  tidyr[...],
  modeest[mlv],
  weights[wtd.cors]
)

library(atlas.plot)

# endregion
# region: data
getOption("atlas.occupations") |>
  read.csv() |>
  as_tibble() ->
df_occupations

# endregion
# models
# region: attribute equivalence 1
aeq <- function(skill_set, generality = NULL) {
  # assert args
  assert$valid_skill_set(skill_set)

  # remove generality from attribute equivalence?
  return(skill_set / max(skill_set))
}

# endregion
# region: attribute equivalence 2
aeq2 <- function(skill_set, generality = NULL) {
  # assert args
  assert$valid_skill_set(skill_set)

  fun_eqvl_logistic <- function(x, a = 0, k = 1, c = 1, q = 1, m = 0, b = 1, nu = 1) {
    # Arguments validated in main functions

    # Generalized logistic function
    y <- a + (k - a) / ((c + q * exp(-b * (x - m)))^(1 / nu))

    # output
    return(y)
  }

  # define variable and midpoint
  skill_set -> x
  (1 - gn$gene(skill_set)) -> m

  rm(skill_set)

  # calculate attribute equivalence
  # with generalized logistic function
  fun_eqvl_logistic(
    x = x,
    m = m,
    a = 0,
    k = x,
    c = 1,
    q = m * (1 - x),
    nu = x / m,
    b = 1 / (1 - m)
  ) -> dbl_attribute_eqvl

  rm(x, m)

  return(dbl_attribute_eqvl)
}

# endregion
# region: skill set competence 1
comp1 <- function(skill_set, generality = NULL) {
  # assert args
  assert$valid_skill_set(skill_set)

  # return skill set competence
  return(
    weighted.mean(
      x = skill_set,
      w = skill_set |> aeq()
    )
  )
}

# endregion
# region: skill set competence 2
comp2 <- function(skill_set, generality = NULL) {
  # assert args
  assert$valid_skill_set(skill_set)

  # return skill set competence
  return(
    weighted.mean(
      x = skill_set,
      w = skill_set |> aeq2()
    )
  )
}

# endregion
# region: skill set competence 3
comp3 <- function(skill_set, generality = NULL) {
  # assert args
  assert$valid_skill_set(skill_set)

  # return skill set competence
  return(
    weighted.mean(
      x = skill_set,
      w = skill_set
    )
  )
}

# endregion
# region: skill set competence 4
comp4 <- function(skill_set, generality = NULL) {
  # assert args
  assert$valid_skill_set(skill_set)

  # skill set's most likely value (mode)
  skill_set_mode <- mlv(skill_set, method = "shorth")

  # return skill set competence
  return(
    skill_set_mode - weighted.mean(
      x = skill_set_mode - skill_set,
      w = skill_set |> aeq()
    )
  )
}

# endregion
# region: skill set competence 5
comp5 <- function(skill_set, generality = NULL) {
  # assert args
  assert$valid_skill_set(skill_set)

  # skill set's most likely value (mode)
  skill_set_mode <- mlv(skill_set, method = "shorth")

  # return skill set competence
  return(
    skill_set_mode - weighted.mean(
      x = skill_set_mode - skill_set,
      w = skill_set |> aeq2()
    )
  )
}

# endregion
# region: skill set competence 6
comp6 <- function(skill_set, generality = NULL) {
  # assert args
  assert$valid_skill_set(skill_set)

  # return skill set competence
  return(
    prod(
      skill_set^((skill_set |> aeq()) / (skill_set |> aeq() |> sum()))
    )
  )
}

# endregion
# region: skill set competence 7
comp7 <- function(skill_set, generality = NULL) {
  # assert args
  assert$valid_skill_set(skill_set)

  # return skill set competence
  return(
    prod(
      skill_set^((skill_set |> aeq2()) / (skill_set |> aeq2() |> sum()))
    )
  )
}

# endregion
# results
# region: competence and generality
df_occupations |>
  select(
    occupation,
    employment_norm,
    starts_with("skl_"),
    starts_with("abl_"),
    starts_with("knw_")
  ) |>
  pivot_longer(
    cols = -c(occupation, employment_norm),
    names_to = "item",
    values_to = "item_score"
  ) |>
  group_by(occupation) |>
  reframe(
    competence1 = comp1(item_score / 100),
    competence2 = comp2(item_score / 100),
    competence3 = comp3(item_score / 100),
    competence4 = comp4(item_score / 100),
    competence5 = comp5(item_score / 100),
    competence6 = comp6(item_score / 100),
    competence7 = comp7(item_score / 100),
    generality = gn$gene(item_score / 100),
    across(
      .cols = -starts_with("item"),
      .fns = first
    )
  ) -> df_model

# endregion
# region: competence vs generality correlation
df_model |>
  select(
    starts_with("comp"),
    starts_with("gene"),
  ) |>
  wtd.cors(
    weight = df_model$employment_norm
  ) |>
  as_tibble(
    rownames = "metric"
  ) -> df_correlations

df_correlations

# endregion
# plots
# region: competence distribution
df_model |>
  pivot_longer(
    cols = starts_with("comp"),
  ) |>
  fun_plot.density(
    aes(
      x = value,
      fill = name
    ),
    .list_geom.param = list(
      bw = 0.03,
      alpha = 0.77,
      size = 1.23
    ),
    .list_axis.x.args = list(
      breaks = seq(0, 1, length.out = 7),
      limits = c(-.25, 1.25)
    ),
    .fun_format.x = percent
  )

# endregion
# region: competence difference distribution
df_model |>
  mutate(
    diff = competence1 - competence2
  ) |>
  fun_plot.histogram(
    aes(x = diff),
  )

# endregion
# region: generality vs competence distribution
df_model |>
  pivot_longer(
    cols = -c(occupation, employment_norm),
  ) |>
  filter(
    name == "competence1" |
      name == "competence2" |
      name == "competence7" |
      name == "generality"
  ) |>
  fun_plot.density(
    aes(
      x = value,
      fill = name,
      weights = employment_norm
    ),
    .list_geom.param = list(
      bw = 0.03,
      alpha = 0.77,
      size = 1.23
    ),
    .list_axis.x.args = list(
      breaks = seq(0, 1, length.out = 7),
      limits = c(-.25, 1.25)
    ),
    .fun_format.x = percent,
    .list_labs = list(
      title = "Skill Set Generality vs Competence Distributions",
      subtitle = "Weighted densities of skill set generality and competence",
      x = "Value",
      y = "",
      fill = ""
      # caption = "Note: competence is the overall magnitude of one's skill level."
    )
  ) + theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

# endregion
# region: generality vs competence 1 scatter plot
df_model |>
  group_by(occupation) |>
  slice(rep(1, employment_norm)) |>
  fun_plot.scatter(
    aes(
      x = competence1,
      y = generality,
      color = employment_norm
    ),
    .list_smooth.param = list(
      method = "lm",
      color = "#212121",
      size = 1.23
    ),
    .scale_colors = scale_color_viridis(
      option = "viridis",
      discrete = F
    ),
    .fun_format.x = percent,
    .fun_format.y = percent,
    .dbl_limits.x = c(0, 1),
    .dbl_limits.y = c(0, 1),
    .list_labs = list(
      title = "Skill Set Generality vs Competence (1)",
      subtitle = "Comparing the overall skill of generalists and specialists",
      x = "Competence",
      y = "Generality",
      color = "Employment Levels",
      caption = "Note: according to this metric, it seems specialists are not, in fact, more skilled than generalists."
    )
  )

# endregion
# region: generality vs competence 2 scatter plot
df_model |>
  group_by(occupation) |>
  slice(rep(1, employment_norm)) |>
  fun_plot.scatter(
    aes(
      x = competence2,
      y = generality,
      color = employment_norm
    ),
    .list_smooth.param = list(
      method = "lm",
      color = "#212121",
      size = 1.23
    ),
    .scale_colors = scale_color_viridis(
      option = "viridis",
      discrete = F
    ),
    .fun_format.x = percent,
    .fun_format.y = percent,
    .dbl_limits.x = c(0, 1),
    .dbl_limits.y = c(0, 1),
    .list_labs = list(
      title = "Skill Set Generality vs Competence (2)",
      subtitle = "Comparing the overall skill of generalists and specialists",
      x = "Competence",
      y = "Generality",
      color = "Employment Levels",
      caption = "Note: it seems specialists are, in general, more skilled than generalists."
    )
  )

# endregion
# region: generality vs competence heatmap
df_correlations |>
  pivot_longer(
    cols = -1
  ) |>
  fun_plot.heatmap(
    aes(
      x = metric,
      y = name,
      fill = value,
      label = value |> percent(accuracy = .01)
    ),
    .scale_colors = scale_fill_gradient2(
      low = "red",
      mid = "white",
      high = "blue"
    ),
    .list_labels.param = list(
      color = "#212121"
    )
  )

# endregion
# use competence metrics 2 or 5 (both are equivalent)
