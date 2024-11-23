# setup
# region: imports
# cran packages
chr_pkg <- c(
  "devtools" # gitHub packages
  , "readr" # read and write data
  , "tidyr", "dplyr", "stringr", "scales" # data wrangling
  , "weights" # fast weighted correlation
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
df_occupations %>%
  select(
    occupation,
    employment_variants,
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

# endregion
# region: generality vs competence correlation
wtd.cors(
  x = df_model$generality,
  y = df_model$competence,
  weight = df_model$employment_variants
) -> dbl_correlation

# endregion
# plots
# region: generality distribution
df_model |>
  fun_plot.density(aes(
    x = generality,
    weights = employment_variants
  ))

# endregion
# region: competence distribution
df_model |>
  fun_plot.density(aes(
    x = competence,
    weights = employment_variants
  ))

# endregion
# region: generality vs competence scatter plot
df_model |>
  fun_plot.scatter(
    aes(
      x = competence,
      y = generality
    ),
    .list_smooth.param = list(
      method = "lm",
      color = "#212121",
      size = 1.23
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
