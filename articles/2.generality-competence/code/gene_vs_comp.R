# setup
# region: imports
# cran packages
chr_pkg <- c(
  # gitHub packages
  "devtools",
  # read and write data
  "readr",
  # data wrangling
  "tidyr", "dplyr", "stringr", "scales",
  # fast weighted correlation
  "weights"
)

# github packages
chr_git <- c(
  "CaoBittencourt" = "atlas.gene",
  "CaoBittencourt" = "atlas.comp",
  "CaoBittencourt" = "atlas.plot"
)

# activate / install CRAN packages
lapply(
  chr_pkg,
  function(pkg) {
    if (!require(pkg, character.only = T)) {
      install.packages(pkg, dependencies = T)
    }

    require(pkg, character.only = T)
  }
)

# activate / install github packages
Map(
  function(git, profile) {
    if (!require(git, character.only = T)) {
      install_github(
        paste0(profile, "/", git),
        dependencies = T,
        upgrade = F,
        force = T
      )
    }

    require(git, character.only = T)
  },
  git = chr_git,
  profile = names(chr_git)
)

# endregion
# region: data
# working directory
getwd() |>
  file.path(
    "papers",
    "2.generality-competence"
  ) |>
  setwd()

# occupations data frame
df_occupations <- read_csv("data/occupations_2022.csv")

# attribute names
df_attribute_names <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vStiX7PF7ltnjqVTCLJchWtaAW_BhVCwUM1hRCXGolCOLCS8mCygyde6FfhcvhZiAvke-tujkTBpXoA/pub?gid=0&single=true&output=csv")

# endregion
# model
# region: generality and competence
df_occupations |>
  select(
    occupation,
    employment_norm,
    starts_with("skl_"),
    starts_with("abl_"),
    starts_with("knw_")
  ) |>
  pivot_longer(
    cols = c(
      starts_with("skl_"),
      starts_with("abl_"),
      starts_with("knw_")
    ),
    names_to = "item",
    values_to = "item_score"
  ) |>
  group_by(across(!c(
    item, item_score
  ))) |>
  reframe(
    generality = fun_gene_generality(item_score),
    competence = fun_comp_competence(item_score),
    across(
      .cols = -starts_with("item"),
      .fns = first
    )
  ) -> df_model

df_model

# endregion
# region: generality vs competence correlation
wtd.cors(
  x = df_model$generality,
  y = df_model$competence,
  weight = df_model$employment_norm
) -> dbl_correlation

dbl_correlation

# endregion
# plots
# region: generality distribution
df_model |>
  fun_plot.density(
    aes(
      x = generality,
      weights = employment_norm
    ),
    .list_axis.x.args = list(
      breaks = seq(0, 1, length.out = 7),
      limits = c(-.1, 1.1)
    ),
    .fun_format.x = percent,
    .list_labs = list(
      title = "Skill Set Generality Distribution",
      subtitle = "Weighted density of skill set generality",
      x = "Generality",
      y = "",
      caption = "Note: generality is the degree to which one employs all attributes in their skill set."
    )
  )

# endregion
# region: competence distribution
df_model |>
  fun_plot.density(
    aes(
      x = competence,
      weights = employment_norm
    ),
    .list_axis.x.args = list(
      breaks = seq(0, 1, length.out = 7),
      limits = c(-.1, 1.1)
    ),
    .fun_format.x = percent,
    .list_labs = list(
      title = "Skill Set Competence Distribution",
      subtitle = "Weighted density of skill set competence",
      x = "Competence",
      y = "",
      caption = "Note: competence is the overall magnitude of one's skill level."
    )
  )

# endregion
# region: generality vs competence distribution
df_model |>
  pivot_longer(
    cols = c(
      generality,
      competence
    ),
  ) |>
  fun_plot.density(
    aes(
      x = value,
      fill = name,
      weights = employment_norm
    ),
    .list_axis.x.args = list(
      breaks = seq(0, 1, length.out = 7),
      limits = c(-.1, 1.1)
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
# region: generality vs competence scatter plot
df_model |>
  group_by(occupation) |>
  slice(rep(1, employment_norm)) |>
  fun_plot.scatter(
    aes(
      x = competence,
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
      title = "Skill Set Generality vs Competence",
      subtitle = "Comparing the overall skill of generalists and specialists",
      x = "Competence",
      y = "Generality",
      color = "Employment Levels",
      caption = "Note: it seems specialists are, in general, more skilled than generalists."
    )
  )

# endregion
# results
# region: descriptive statistics

# endregion
# region: generality table

# endregion
# region: competence table

# endregion
# region: attributes table

# endregion
# export
# region: save rds file

# endregion
# region: LaTeX tables

# endregion
